<?php
require('validEmail.php');
require('trialLicenseDB.php');
require_once('recaptchalib.php');

$recaptchaPrivateKey = '6Le7cAwAAAAAAGSJo41Q2Huz6UuUYahENmSn1ug4';

// Form fields
$name = $_POST['customerName'];
$email = $_POST['customerEmail'];
$usertype = $_POST['usertype'];
$background = $_POST['background'];
$usage = $_POST['usage'];
$remoteAddr = $_SERVER['REMOTE_ADDR'];
$challengeField = $_POST['recaptcha_challenge_field'];
$responseField = $_POST['recaptcha_response_field'];

function validateFields($db, $name, $email){
  if(!validEmail($email)){
    $db->recordError(2, $name, $email);
    logError("You must enter a valid email address.");
  }
  if(!$db->uniqueEmail($email)){
    $db->recordError(3, $name, $email);
    logError("A trial license was already issued to you.  If you would like to extend your trial please contact support@simatratechnologies.com.");
  }
  if($name == ""){
    $db->recordError(4, $name, $email);
    logError("You must enter your name.");
  }
}

function logError($message){
  header("Location: trial.php?error=" . urlencode($message));
  exit(0);
}

// Open the license database
$db = new trialLicenseDB();
if(!$db->isValid()){
  logError('Internal Database Error. '. $db->getError());
}

// Check for a valid reCaptcha answer
$captcha = recaptcha_check_answer($recaptchaPrivateKey,
				  $remoteAddr,
				  $challengeField,
				  $responseField);
if(!$captcha->is_valid){
  // Log error to database
  $db->recordError(1, $name, $email);
  // Report error to user
  logError('Failed to correctly enter the reCAPTCHA words. Please try again.');
}

// Check for valid fields
validateFields($db, $name, $email);

// Add trial request to database
$trialid = $db->recordTrialLicense($name, $email, $usertype, $background, $usage);

if(!$trialid){
  // Report error to user
  logError('Internal Database Error. ' . $db->getError());
}

// Generate a license key and email to user
$escname = addslashes($name);
$escemail = addslashes($email);


exec("./trialLicense.cgi $trialid \"$escname\" \"$escemail\"", $licenseKey);

$body = "The six lines below are the trial license key for simEngine.  " .
  "You may copy and paste them into your Matlab session during installation, ". 
  "or by running simCheckLicense('-update') from within Matlab.\n\n";

foreach($licenseKey as $line){
  $body = $body . $line . "\n";
}

if(mail($email,"simEngine Trial License Key", $body, "From: \"Simatra Modeling Technologies\" <support@simatratechnologies.com>")){
  echo("<p>Your trial license has been sent to you via email.</p>");
}
else{
  logError("There was an error delivering your trial license.");
}

?>