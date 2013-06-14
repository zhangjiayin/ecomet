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
    oneway void send(1:string AppId, 2:string Id, 3:string Msg),
    i32 get_online_count(1:string AppId),
    list<string> get_online_ids(1:string AppId)
}
