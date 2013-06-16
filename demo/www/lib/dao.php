<?php

class Demo_Dao {
    public $dsn = 'sqlite:user.db';
    public $user = 'user';
    public $password = 'password';
    public $initSql = <<<EOB
CREATE TABLE user ( 
  id INTEGER PRIMARY KEY AUTOINCREMENT ,
  name VARCHAR( 32) NOT NULL,
  password VARCHAR( 64) NOT NULL
);
EOB;
    public static $db;

    public function getDb(  ) {
        if( empty( self::$db ) ) {
            self::$db = new PDO( $this->dsn, $this->user, $this->password);
        }

        return self::$db;
    }


    public function addUser($user, $password  ) {
       $stmt = $this->getDb( )->prepare( 'INSERT INTO user( name,password) VALUES ( ?, ?)' );
       $stmt->execute(array(  $user, $password ));
       $id = $this->getDb(  )->lastInsertId(  );
       if( !empty( $id ) ) {
           return array( 'id' => $id , 'name' => $user , 'password' => $password );
       }
       return $id;
    }

    public function getUser( $user ) {
       $stmt = $this->getDb()->prepare( 'SELECT * FROM  user where name = :user' );
       $stmt->bindParam( ':user', $user, PDO::PARAM_STR);
       $stmt->execute(  );
       return $stmt->fetch( PDO::FETCH_ASSOC );
    }

    public function getUsers( $ids ) {
       $stmt = $this->getDb()->prepare( 'SELECT id,name FROM  user where id in ( ' . implode( ',', $ids ) . ' )' );
       $stmt->execute(  );
       return $stmt->fetchAll( PDO::FETCH_ASSOC );
    }

    public function authUser($user, $password  ) {
       $stmt = $this->getDb()->prepare( 'SELECT * FROM  user where name = :user  AND password = :password' );
       $stmt->bindParam( ':user', $user, PDO::PARAM_STR);
       $stmt->bindParam( ':password', $password, PDO::PARAM_STR);
       $stmt->execute(  );
       return $stmt->fetch( PDO::FETCH_ASSOC );
    }

    public function initDb ( ) {
        return $this->getDb(  )->exec( $this->initSql );
    }
}

//$a = new Demo_Dao(  );
//echo $a->addUser( "111","111" );
//$r = $a->authUser( "111","111" );
//$r = $a->getUser( "111");
//var_export( $r );
//print_r($a->getDb(  )->errorInfo(  ));
