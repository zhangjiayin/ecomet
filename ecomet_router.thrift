namespace cpp etao.erouter
namespace java etao.erouter
namespace php  etao.erouter

/**
struct Message {
       1: i32 created,
       2: i32 expire = 0,
       3: string content,
}
*/


service EcometRouter {
    oneway void send(1:i64 AppId, 2:i64 Id, 3:string Msg, 4:bool Offline=false),
    i64 get_online_count(1:i32 AppId),
    list<i64> get_online_ids(1:i32 AppId)
}
