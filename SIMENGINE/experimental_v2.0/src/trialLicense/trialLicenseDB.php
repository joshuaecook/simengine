<?
class trialLicenseDB
{
  private $db, $error;

  public function __construct()
  {
    try{
      //create or open the database
      $this->db = new PDO('sqlite:/home/simatrat/db/simEngineTrialLicenses.db');

      // Create the TrialLicenseUsers table
      $query = 'CREATE TABLE TrialLicenseUsers ' .
	'(trialid INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT, Email TEXT, DateTime TEXT, IPaddr TEXT)';
      $this->db->exec($query);

      // Create the TrialLicenses table
      $query = 'CREATE TABLE TrialLicenses ' .
	'(trialid INTEGER, licenseKey TEXT)';
      $this->db->exec($query);

      // Create the Questions1 table
      $query = 'CREATE TABLE Questions1 ' .
	'(trialid INTEGER, UserType TEXT, Background TEXT, Usage TEXT)';
      $this->db->exec($query);      

      // Create the TrialErrors table
      $query = 'CREATE TABLE TrialErrors ' .
	'(errorid INTEGER PRIMARY KEY AUTOINCREMENT, errorcode INTEGER, Name TEXT, Email TEXT, DateTime TEXT, IPaddr TEXT)';
      $this->db->exec($query);
    }
    catch(Exception $e){
      $this->error = $e->getMessage();
      $this->db = null;
    }
  }

  public function isValid(){
    return $this->db != null;
  }

  public function getError(){
    return $this->error;
  }

  public function uniqueEmail($email)
  {
    if($this->db){
      $sqlemail = sqlite_escape_string($email);
      $query = "SELECT Email from TrialLicenseUsers where Email = \"$sqlemail\"";

      foreach($this->db->query($query) as $row){
	return false;
      }
      return true;
    }
    else{
      return false;
    }
  }

  public function uniqueName($name)
  {
    if($this->db){
      $sqlname = sqlite_escape_string($name);
      $query = 'SELECT Name from TrialLicenseUsers where Name = "$sqlname"';

      return !$this->db->query($query);
    }
    else return false;
  }

  public function recordTrialLicense($trialid, $licenseKey){
    if($this->db){
      $sqllicenseKey = sqlite_escape_string($licenseKey);
      $query = "INSERT INTO TrialLicenses (trialid, licenseKey) " .
	"VALUES ($trialid, \"$sqllicenseKey\");";
      try{
	$this->db->exec($query);
      }
      catch(Exception $e){
	$this->error = $e->getMessage();
	return 0;
      }
    }
    return 1;
  }

  public function recordTrialLicenseUser($name, $email, $usertype, $background, $usage)
  {
    $trialid = 0;
    if($this->db){
      //insert data into database
      $sqlname = sqlite_escape_string($name);
      $sqlemail = sqlite_escape_string($email);
      $sqlusertype = sqlite_escape_string($usertype);
      $sqlbackground = sqlite_escape_string($background);
      $sqlusage = sqlite_escape_string($usage);
      $timestamp = $this->dateTimeNow();
      $ipaddr = $_SERVER['REMOTE_ADDR'];

      $query = "INSERT INTO TrialLicenseUsers (Name, Email, DateTime, IPaddr) " .
	"VALUES (\"$sqlname\", \"$sqlemail\", \"$timestamp\", \"$ipaddr\");";
      try{
	$this->db->exec($query);
      }
      catch(Exception $e){
	$this->error = $e->getMessage();
	return 0;
      }

      $query = "SELECT trialid FROM TrialLicenseUsers WHERE " .
	"Name = \"$sqlname\" and Email = \"$sqlemail\";";
      if($result = $this->db->query($query)){
      	foreach($result as $row){
	  $trialid = $row['trialid'];
        }
      }
      else{
	$this->error = "Could not find the record previously inserted.";
	return 0;
      }

      $query = "INSERT INTO Questions1 (trialid, UserType, Background, Usage) " .
	"VALUES ($trialid, \"$sqlusertype\", \"$sqlbackground\", \"$sqlusage\");";

      try{
	$this->db->exec($query);
      }
      catch(Exception $e){
	$this->error = $e->getMessage();
	return 0;
      }
    }

    return $trialid;
  }

  public function recordError($errorcode, $name, $email)
  {
    if($this->db){
      //insert data into database
      $sqlname = sqlite_escape_string($name);
      $sqlemail = sqlite_escape_string($email);
      $timestamp = $this->dateTimeNow();
      $ipaddr = $_SERVER['REMOTE_ADDR'];

      $query = "INSERT INTO TrialErrors (errorcode, Name, Email, DateTime, IPaddr) " .
	"VALUES ($errorcode, \"$sqlname\", \"$sqlemail\", \"$timestamp\", \"$ipaddr\");";

      try{
	$this->db->exec($query);
      }
      catch(Exception $e){
	$this->error = $e->getMessage();
      }
    }
  }

  public function close(){
    $this->db = null;
    $this->error = "Database closed.";
  }

  //  Protected Functions 
  // Producde a timestamp
  protected function dateTimeNow()
  {
    $datetime = new DateTime('now', new DateTimeZone('America/New_York'));
    return $datetime->format(DateTime::ATOM);
  }
}
?>