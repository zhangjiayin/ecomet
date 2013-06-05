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
    oneway void send(1:string Id, 2:string Msg)
}
