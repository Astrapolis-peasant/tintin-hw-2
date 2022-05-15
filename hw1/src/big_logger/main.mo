import TextLogger "text_logger";
import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
import Logger "mo:ic-logger/Logger";
import List "mo:base/List";

actor BigLogger {
    let logger_size = 100;
    var current_log_number = 0;
    var logger_buckets = Buffer.Buffer<TextLogger.TextLogger>(0);

    public func append(logs : [Text]) : async (){
        var logs_to_append = Buffer.Buffer<Text>(0);
        for (i in Iter.fromArray(logs)) {
            if  (current_log_number%logger_size == 0) {
                if (logs_to_append.size() != 0) {
                    logger_buckets.get(logger_buckets.size()-1).append(logs_to_append.toArray());
                };
                logger_buckets.add(await TextLogger.TextLogger());
                logs_to_append := Buffer.Buffer<Text>(0);
            }; 
            logs_to_append.add(i);  
            current_log_number += 1;
        };
        logger_buckets.get(logger_buckets.size()-1).append(logs_to_append.toArray());
    };

    public func view(start_index : Nat, end_index : Nat) : async Logger.View<Text> {
        assert(start_index <= end_index);
        var logger_bucket = start_index/logger_size;
        var start : Nat = start_index - logger_bucket*logger_size;
        var results : List.List<Text> = List.nil();
        let max_end_index = if (end_index > current_log_number-1) current_log_number-1 else end_index;
        while (logger_bucket < max_end_index/logger_size) { 
            let view = await logger_buckets.get(logger_bucket).view(start, logger_size);
            results := List.append(results, List.fromArray(view.messages));
            logger_bucket += 1;
            start := 0;
        };
        let view = await logger_buckets.get(logger_bucket).view(start, max_end_index);
        results := List.append(results, List.fromArray(view.messages));        
        {start_index=start_index; messages=List.toArray(results)};
    }
}