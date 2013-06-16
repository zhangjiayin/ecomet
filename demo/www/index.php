<?php
session_start( );

define( 'TPL_DIR' , __DIR__ . '/tpl' );

include __DIR__ . '/lib/dao.php';
include __DIR__ . '/lib/client.php';

$dao = new Demo_Dao(  );
$client = new Demo_Client(  );
//first run initDb
//$dao->initDb(  );
if( $_SERVER["REQUEST_METHOD"] == "POST" ){
    $type = isset( $_POST["type"] ) ? $_POST["type"] : '';

    $result = array( "isLogin" => false, "uid" => "0", "error" => "");

    if( !empty( $_SESSION["user"] )  && !empty( $_SESSION["user"]["id"] )) {
        $result["isLogin"] = true;
        $result["uid"] = $_SESSION["user"]["id"];
    }
    switch( $type ) {
    case "get_online_users":
        $users = $client->getOnlineUsers(  );
        $users = array_unique( $users );
        $result["users"] = array(  );
        if( !empty( $users ) ){
            $result["users"] = $dao->getUsers( $users );
        }
        break;
    case "send":
        if( $_POST["to"] == "broadcast" ) {
            $users = $client->getOnlineUsers(  );
            $users = array_unique( $users );
            foreach( $users as $uid ) {
                $result["error"] = $client->sendMsg($_SESSION["user"]["name"], $_SESSION["user"]["id"], $uid,$_POST["msg"] , 'broadcast');
            }
        } else {
            $result["error"] = $client->sendMsg($_SESSION["user"]["name"], $_SESSION["user"]["id"], $_POST["to"],$_POST["msg"] );
        }
        break;
    case "login":
        $username = isset( $_POST["username"] ) ? $_POST["username"] : "";
        $password = isset( $_POST["password"] ) ? $_POST["password"] : "";
        if( ( $user = $dao->authUser( $username, $password )) ) {
            $result["uid"] = $user["id"];
            $result["isLogin"] = true;
            $_SESSION["user"] = $user;
        } else if( $user = $dao->getUser( $username ) ) {
            $result["error"] = "密码错误";
        } else {
            $user = $dao->addUser( $username, $password );
            $_SESSION["user"] = $user;
            $result["uid"] = $user["id"];
            $result["isLogin"] = true;
        }
        break;
        default:
    }
    echo json_encode( $result );
    exit;
}
include TPL_DIR .  '/signin.php';
