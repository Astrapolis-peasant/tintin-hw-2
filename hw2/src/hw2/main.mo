import Buffer "mo:base/Buffer";
import H "mo:base/TrieMap";
import IC "./ic";
import List "mo:base/List";
import Principal "mo:base/Principal";
import Float "mo:base/Float";
import Result "mo:base/Result";

shared(msg) actor class () =  self {
  public type Action = {
    #create_canister; 
    #start_canister : IC.canister_id; 
    #install_code : InstallCodeParams; 
    #stop_canister : IC.canister_id; 
    #delete_canister : IC.canister_id;
  };

  type ProposalContent = {
    proposal_id : Nat;
    var agreed : List.List<Principal>;
    action : Action;
    desc: Text;
    var approved : Bool;
  };

  public type ProposalView = {
    proposal_id : Nat;
    agreed : [Principal];
    action : Action;
    desc: Text;
    approved : Bool;
    canister_status : ?CanisterStatus;
  };

  public type InstallCodeParams = {
    arg : [Nat8];
    wasm_module : IC.wasm_module;
    mode : { #reinstall; #upgrade; #install };
    canister_id : IC.canister_id;
  };

  type CanisterStatus = {
     #stopped; #stopping; #running; #deleted ;
  };

  var approve_threshold = 0.5;
  let controllers = Buffer.Buffer<Principal>(1);
  let proposals = Buffer.Buffer<ProposalContent>(1);
  let ic : IC.Self = actor("aaaaa-aa");
  var canister_statuses =  H.TrieMap<Principal, CanisterStatus>(Principal.equal, Principal.hash);

  public shared (msg) func add_controller() : async (){
    controllers.add(msg.caller)
  };

  // threadhost is from 0 - 10, 
  public shared (msg) func set_approve_threshold(threshold : Float) : async (){
    approve_threshold := threshold;
  };

  public shared (msg) func propose(action : Action, desc : Text) : async (ProposalView){
    let proposal_content : ProposalContent =  {
        proposal_id = proposals.size();
        var agreed : List.List<Principal> = List.nil();
        action = action;
        desc = desc;
        var approved = false;
      };
    proposals.add(proposal_content);
    get_proposal_view(proposal_content, null)
  };

  func get_proposal_view(proposal : ProposalContent, status: ?CanisterStatus) : (ProposalView){
    {proposal_id = proposal.proposal_id; 
     agreed = List.toArray(proposal.agreed); 
     action = proposal.action; 
     desc = proposal.desc; 
     approved = proposal.approved;
     canister_status = status;      
    }
  };

  public shared (msg) func approve(proposal_id : Nat) : async (?ProposalView){
    var proposal : ?ProposalContent = proposals.getOpt(proposal_id);
    switch proposal {
      case null {null};
      case (?proposal) {
        proposal.agreed := List.push(msg.caller, proposal.agreed);
        proposal.approved := Float.fromInt(List.size(proposal.agreed))/Float.fromInt(controllers.size()) >= approve_threshold;
        if (proposal.approved == true) {
          switch (proposal.action) {
            case (#create_canister) {
              let settings = {
                freezing_threshold = null;
                controllers = ?[Principal.fromActor(self)];
                memory_allocation = null;
                compute_allocation = null;
              };
              let result = await ic.create_canister({settings = ?settings});
              canister_statuses.put(result.canister_id, #stopped);              
              ?get_proposal_view(proposal, ?#stopped)
            };
            case (#start_canister(canister_id)) {
              await ic.start_canister({ canister_id });
              canister_statuses.put(canister_id, #running);              
              ?get_proposal_view(proposal, ?#running)
            };
            case (#install_code(install_code_params)) {
              await ic.install_code( install_code_params );
              canister_statuses.put(install_code_params.canister_id, #running);              
              ?get_proposal_view(proposal, ?#running)
            };
            case (#stop_canister(canister_id)) {
              await ic.stop_canister({ canister_id });
              canister_statuses.put(canister_id, #stopped);              
              ?get_proposal_view(proposal, ?#stopped)
            };
            case (#delete_canister(canister_id)) {
              await ic.delete_canister({ canister_id });
              canister_statuses.put(canister_id, #deleted);              
              ?get_proposal_view(proposal, ?#deleted)
            };                                 
          }
        }
        else {
          ?get_proposal_view(proposal, null)
        }
      }
    }
  };
}
