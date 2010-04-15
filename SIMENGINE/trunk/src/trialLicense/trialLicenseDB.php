<?
class trialLicenseDB
{
  private $db, $error;

  public function __construct()
  {
    try{
      //create or open the database
      $this->db = new PDO('sqlite:/home/simatrat/db/simEngineTrialLicenses.db');
      // Create the TrialLicenses table
      $query = 'CREATE TABLE TrialLicenses ' .
	'(trialid INTEGER PRIMARY KEY AUTOINCREMENT, Name TEXT, Email TEXT, UserType TEXT, Background TEXT, Usage TEXT, DateTime TEXT)';
      $this->db->exec($query);

      // Create the TrialErrors table
      $query = 'CREATE TABLE TrialErrors ' .
	'(errorid INTEGER PRIMARY KEY AUTOINCREMENT, errorcode INTEGER, Name TEXT, Email TEXT, DateTime TEXT)';
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
      $query = "SELECT Email from TrialLicenses where Email = \"$sqlemail\"";

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
      $query = 'SELECT Name from TrialLicenses where Name = "$sqlname"';

      return !$this->db->query($query);
    }
    else return false;
  }

  public function recordTrialLicense($name, $email, $usertype, $background, $usage)
  {
    if($this->db){
      //insert data into database
      $sqlname = sqlite_escape_string($name);
      $sqlemail = sqlite_escape_string($email);
      $sqlusertype = sqlite_escape_string($usertype);
      $sqlbackground = sqlite_escape_string($background);
      $sqlusage = sqlite_escape_string($usage);
      $timestamp = $this->dateTimeNow();

      $query = "INSERT INTO TrialLicenses (Name, Email, UserType, Background, Usage, DateTime) " .
	"VALUES (\"$sqlname\", \"$sqlemail\", \"$sqlusertype\", \"$sqlbackground\", \"$sqlusage\", \"$timestamp\");";
      try{
	$this->db->exec($query);
      }
      catch(Exception $e){
	$this->error = $e->getMessage();
	return 0;
      }

      $query = "SELECT trialid FROM TrialLicenses WHERE " .
	"Name = \"$sqlname\" and Email = \"$sqlemail\";";
      if($result = $this->db->query($query)){
      	foreach($result as $row){
	  $trialid = $row['trialid'];
        }
	return $trialid;
      }
      else{
	$this->error = "Could not find the record previously inserted.";
	return 0;
      }
    }
  }

  public function recordError($errorcode, $name, $email)
  {
    if($this->db){
      //insert data into database
      $sqlname = sqlite_escape_string($name);
      $sqlemail = sqlite_escape_string($email);
      $timestamp = $this->dateTimeNow();
      $query = "INSERT INTO TrialErrors (errorcode, Name, Email, DateTime) " .
	"VALUES ($errorcode, \"$sqlname\", \"$sqlemail\", \"$timestamp\");";

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