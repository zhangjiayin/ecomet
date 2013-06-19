$(function(){
    $.ajax({
        type: "POST",
        url: '/',
        data: {type:"isLogin"},
        error: function(data) {
        },
        success: function(data) {
            if(data.uid !="0") {
                startChat(data.uid);
                $("#chatDiv").fadeToggle("fast");
            } else {
                $("#loginForm").fadeToggle("fast");
            }
        },
        dataType: 'json' 
    });
    
    $("#username").focus(function(){
        $("#errorInfo").html("");
    });

    $("#password").focus(function(){
        $("#errorInfo").html("");
    });
    $("#loginBtn").click(function(){
        var username = $("#username").val().trim();   
        var password = $("#password").val().trim();   
        if(username == "") {
            $("#errorInfo").css("color","red");
            $("#errorInfo").html("用户名不能为空");
            return;
        }

        if(password == "") {
            $("#errorInfo").css("color","red");
            $("#errorInfo").html("没有输入密码");
            return;
        }

        $.ajax({
            type: "POST",
            url: '/',
            data: {type:"login",username:username,password:password},
            success: function(data) {
                if(data.error != ""){
                    $("#errorInfo").css("color","red");
                    $("#errorInfo").html(data.error);
                    return;
                } 

                if(data.uid !="0") {
                    $("#loginForm").fadeToggle("fast");
                    $("#chatDiv").fadeToggle("fast");
                    startChat(data.uid);
                }

            },
            dataType: 'json' 
        });
    });

    var uid = 0;
    function startChat(u){
        uid = u;
        longPoll();
    }

    var retry_time = 1;

    function longPoll() {
        $.ajax({
            type: "POST",
            url: "http://demo:8080/longpoll/1?" + new Date(),
            data: {uid:uid},
            dataType: "json",
            success: function(retData){ 
                if($.isArray(retData)) {
                    $.each(retData,function(index,ret){
                    if(typeof(ret.type) =="undefined" || ret.type == "msg")  {
                        initChatTab(ret.nick,ret.from);
                        var title = ret.nick + "说: ";
                        addchatMsg(ret.from, title, ret.content, 1);
                    }  else if(ret.type == "broadcast") {
                        initChatTab('所有人','broadcast');
                        var title = ret.nick + "说: ";
                        addchatMsg(ret.type, title, ret.content, 1);
                    }
                    retry_time = retry_time / 2;
                    if(retry_time < 1) {
                        retry_time = 1;
                    }
                    });
                }
                setTimeout(longPoll, 1);
            },
            error : function() {
                retry_time = retry_time * 2;
                setTimeout(longPoll, retry_time*1000);
            }
        });
    }


    function getOnlineUsers() {
        $.ajax({
            type: "post",
            url: "/?" + new Date(),
        data: {type:"get_online_users"},
        dataType: 'json',
        success: function(data){ 
            //TODO remove offline user
            var online_ids = [];
           $.each(data["users"],function(index,item){
               online_ids.push(item.id);
               addOnlineUser(item.id, item.name);
               $('#user_' + item.id).find("button").find('font').remove();
           });
           //TODO UNCOMMET
          setTimeout(getOnlineUsers, 10000);
           $('#userDiv div.btn-group').each(function(index,item) {
           if( $(item).attr('id').substring(5)!= 'broadcast' && $.inArray($(item).attr('id').substring(5),online_ids) === -1) {

               //离线了删除
               if($(item).find("button").find('font').length == 0 ){
                var f=   $('<font>不在线</font>');
                f.css("font-size", "10");
                console.log(f);
                f.appendTo($(item).find("button")[0]);
               }
               
           }
           });
        },
        error: function() {
                   setTimeout(getOnlineUsers, retry_time  * 1000);
               }
        });
    }
    
    $('.dropdown-toggle').dropdown();
    getOnlineUsers();

    $('#chatTab a').click(function (e) {
        e.preventDefault();
        $(this).tab('show');
    })

    $('#chatTab a:first').tab('show');

    addOnlineUser('broadcast', '所有人');
});

function initChatTab(name, uid) {
    if($('#chatTab_' + uid).length > 0)return;

    var tab = $('<li id="chatTab_' +  uid + '"><a href="#chatContent_' + uid + '">' + "和"+ name + "的对话" + '</a></li>');
    tab.find("a").click(function (e) {
        $(this).css('background-color','');
        $(this).parent().css('background-color','');
        e.preventDefault();
        $(this).tab('show');
    });
    tab.appendTo($("#chatTab"));
    var con = $('<div class="tab-pane" id="chatContent_' + uid + '"></div>');
    if($("#chatContent").find('div').length == 0){
        con.addClass('active');
    }
    con.appendTo($("#chatContent"));
    
}

function addchatMsg(id,title, msg,isnew) {
    var chat = $('<div class="prettyprint linenums"></div>');
    chat.html(title  + '<br />' + msg + '<br />');
    chat.appendTo($("#chatContent_" + id));
    if(typeof(isnew) !== "undefined") {
        $('#chatTab_' + id).css('background-color','blue');
        $('#chatTab_' + id + ' a').css('background-color','blue');
    }
}

function sendMsg(from, to, name,  msg) {
    initChatTab(name, to);
    $('#chatTab_' + to).find('a').tab('show');
    $('#chatTab_' + to ).css('background-color','');
    $('#chatTab_' + to ).find('a').css('background-color','');
    var title = "对" + name +" 说:";

    if(to != "broadcast") {
        addchatMsg(to,title, msg);
    }
    $.post(
        "/",
        {type:'send','from':from,'to':to, 'msg':msg},
        function(result){
        });
}

function addOnlineUser(uid, name) {

    if($("#user_" + uid).length > 0){
        return;
    }
    var userHtml = '';
    userHtml += '<div class="btn-group">';
    userHtml +=  '<button class="btn btn-large">' + name + '</button>';
    userHtml +=  '<button class="btn btn-large dropdown-toggle" data-toggle="dropdown"><span class="caret"></span></button>';
    userHtml +=  '<ul class="dropdown-menu">';
    userHtml +=  '<li class="sendBtn"><a>发送消息</a></li>';
    userHtml +=   '</ul>';
    userHtml +=  '</div>';
    userDiv = $(userHtml);
    userDiv.appendTo($('#userDiv'));
    userDiv.attr('id', 'user_' + uid);
    var sendBtn = userDiv.find('li.sendBtn');
    sendBtn.click(function(e){
        var uid = $(this).parent().parent().attr('id').substring(5);
        $("#chatDialog h3").html("发送消息给" + name);
        $('#chatDialog .btn-primary').unbind();
        $('#chatDialog').modal('toggle');
        $('#chatDialog .btn-primary').click(function(){
            sendMsg(uid, uid,  name, $('#chatDialog textarea').val());
            $("#chatDialog textarea").val("");
            $('#chatDialog').modal('toggle');
        });
    });
    $('<div class="clear"></div>').appendTo($('#userDiv'));
}
