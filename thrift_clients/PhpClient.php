#!/usr/bin/env php
<?php

//namespace etao\erouter;

error_reporting(E_ALL);

require_once __DIR__.'/php_lib/Thrift/ClassLoader/ThriftClassLoader.php';

use Thrift\ClassLoader\ThriftClassLoader;

$GEN_DIR = realpath(dirname(__FILE__).'').'/gen-php';
require_once __DIR__.'/gen-php/etao/erouter/EcometRouter.php';

$loader = new ThriftClassLoader();
$loader->registerNamespace('Thrift', __DIR__ . '/php_lib');
$loader->registerDefinition('shared', $GEN_DIR);
$loader->registerDefinition('tutorial', $GEN_DIR);
$loader->register();

/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TSocket;
use Thrift\Transport\THttpClient;
use Thrift\Transport\TBufferedTransport;
use Thrift\Exception\TException;

try {
    $socket = new TSocket('127.0.0.1', 9999);
  $transport = new TBufferedTransport($socket, 1024, 1024);
  $protocol = new TBinaryProtocol($transport);
  $client = new  etao\erouter\EcometRouterClient($protocol);

  $transport->open();
    /**
  for( $i=0;$i<1;$i++ ) {
     */
 $i =0;
$a = json_encode( array(
    "from" =>  "系统消息",
    "msg" => "令小湛" . $i,
    "type" => "auction",
) );
     $client->send(1,'2',$a);
  /*
  }
  */
  $list = $client->get_online_ids(1);
  var_dump( $list );
  $list = $client->get_online_count(1);
  var_dump( $list );

  $transport->close();
    $socket->close( );
} catch (TException $tx) {
  print 'TException: '.$tx->getMessage()."\n";
}

?>
