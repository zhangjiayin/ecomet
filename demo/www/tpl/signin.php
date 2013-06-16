<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8">
        <title>CHAT DEMO</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta name="description" content="">
        <meta name="author" content="">

        <!-- Le styles -->
        <link href="css/bootstrap.css" rel="stylesheet">
        <style type="text/css">
            body {
                padding-top: 40px;
                padding-bottom: 40px;
                background-color: #f5f5f5;
            }

            .form-signin {
                max-width: 300px;
                padding: 19px 29px 29px;
                margin: 0 auto 20px;
                background-color: #fff;
                border: 1px solid #e5e5e5;
                -webkit-border-radius: 5px;
                -moz-border-radius: 5px;
                border-radius: 5px;
                -webkit-box-shadow: 0 1px 2px rgba(0,0,0,.05);
                -moz-box-shadow: 0 1px 2px rgba(0,0,0,.05);
                box-shadow: 0 1px 2px rgba(0,0,0,.05);
            }
            .form-signin .form-signin-heading,
            .form-signin .checkbox {
                margin-bottom: 10px;
            }
            .form-signin input[type="text"],
            .form-signin input[type="password"] {
                font-size: 16px;
                height: auto;
                margin-bottom: 15px;
                padding: 7px 9px;
            }

        </style>
        <link href="css/bootstrap-responsive.css" rel="stylesheet">

        <!-- HTML5 shim, for IE6-8 support of HTML5 elements -->
        <!--[if lt IE 9]>
        <script src="js/html5shiv.js"></script>
        <![endif]-->

        <!-- Fav and touch icons -->
        <link rel="apple-touch-icon-precomposed" sizes="144x144" href="ico/apple-touch-icon-144-precomposed.png">
        <link rel="apple-touch-icon-precomposed" sizes="114x114" href="ico/apple-touch-icon-114-precomposed.png">
        <link rel="apple-touch-icon-precomposed" sizes="72x72" href="ico/apple-touch-icon-72-precomposed.png">
        <link rel="apple-touch-icon-precomposed" href="ico/apple-touch-icon-57-precomposed.png">
        <link rel="shortcut icon" href="ico/favicon.png">
    </head>

    <body>

        <div class="container">
            <form id="loginForm" class="form-signin" style="display:none;">
                <h2 class="form-signin-heading">请登录，如果用户名没有重复，第一次登录相当于注册</h2>
                <input id="username" type="text" class="input-block-level" placeholder="昵称" name="email">
                <input id="password" type="password" class="input-block-level" placeholder="密码" name="email">
                <button id="loginBtn" class="btn btn-large btn-primary" type="button">登录</button>
                <label id="errorInfo">
                </label>
            </form>

            <div class="row" id="chatDiv" style="display:none;">
                <div class="span3 bs-docs-sidebar">
                    <div class="btn-toolbar" id="userDiv">
                    </div><!-- /btn-toolbar -->
                </div>
                <div class="span9">
                        <ul class="nav nav-tabs" id="chatTab">
                        </ul>
                        <div class="tab-content" id="chatContent">
                        </div>
                </div>
            </div>
        </div> <!-- /container -->

        <!-- Le javascript
        ================================================== -->
        <!-- Placed at the end of the document so the pages load faster -->
        <script src="js/jquery-2.0.2.min.js"></script>
        <script src="js/bootstrap.js"></script>
        <script src="js/demo.js"></script>

        <!-- Modal -->
        <div id="chatDialog" class="modal hide fade" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
            <div class="modal-header">
                <button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
                <h3 id="myModalLabel">Modal header</h3>
            </div>
            <div class="modal-body">
                <p><textarea style="width:98%;"></textarea></p>
            </div>
            <div class="modal-footer">
                <button class="btn" data-dismiss="modal" aria-hidden="true">取消</button>
                <button class="btn btn-primary">发送</button>
            </div>
        </div>

    </body>
</html>
