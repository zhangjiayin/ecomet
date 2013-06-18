namespace cpp etao.erouter
namespace java etao.erouter
namespace php  etao.erouter

struct Message {
       1: i32 appId,
       2: i64 from,
       3: i64 to,
       4: string nick="",
       5: string type = "msg",
       6: string content,
       7: i32 created=0,
       8: bool offline=false,
       9: i32 expire=0
}


service EcometRouter {
    oneway void send(1:Message Msg),
    i64 get_online_count(1:i32 AppId),
    list<i64> get_online_ids(1:i32 AppId)
}
