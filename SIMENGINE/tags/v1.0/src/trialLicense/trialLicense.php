<?php
require('validEmail.php');
require('trialLicenseDB.php');
require_once('recaptchalib.php');

// Sole entry point into this file
processTrialLicense();

function processTrialLicense(){
  // Retrieve form values
  $name = postVal('customerName');
  $email = postVal('customerEmail');
  $usertype = postVal('usertype');
  $background = postVal('background');
  $usage = postVal('usage');

  // Open the license database
  $db = new trialLicenseDB();
  if(!$db->isValid()){
    logError('Internal Database Error.'); //. $db->getError());
  }

  // Validate Captcha input
  checkCaptcha($db);

  // Check for valid fields
  validateFields($db, $name, $email);

  // Add trial request to database
  $trialid = $db->recordTrialLicense($name, $email, $usertype, $background, $usage);

  if(!$trialid){
    // Report error to user
    logError('Internal Database Error.'); // . $db->getError());
  }

  mailLicenseKey($trialid, $name, $email);
}

function postVal($var){
  if(get_magic_quotes_gpc())
    return stripslashes($_POST[$var]);
  else
    return $_POST[$var];
}

function checkCaptcha($db){
  $recaptchaPrivateKey = '6Le7cAwAAAAAAGSJo41Q2Huz6UuUYahENmSn1ug4';
  // Form fields
  $remoteAddr = $_SERVER['REMOTE_ADDR'];
  $challengeField = postVal('recaptcha_challenge_field');
  $responseField = postVal('recaptcha_response_field');

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
}

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
  if(checkBlackList($name, $email)){
    $db->recordError(5, $name, $email);
    logError("A trial license could not be created for your.  Please contact support@simatratechnologies.com for assistance.");
  }
}

// Entry point to add future code to deny creation of trial licenses for a specific domain name, etc.
function checkBlackList($name, $email){
  return false;
}

// Report an error in processing user form data (or internal error)
function logError($message){
  $name = postVal('customerName');
  $email = postVal('customerEmail');
  $usertype = postVal('usertype');
  $background = postVal('background');
  $usage = postVal('usage');
  header("Location: download-free-trial-of-simEngine.php?" .
	 "customerName=" . urlencode($name) .
	 "&customerEmail=" . urlencode($email) .
	 "&usertype=" . urlencode($usertype) .
	 "&background=" . urlencode($background) .
	 "&usage=" . urlencode($usage) .
	 "&error=" . urlencode($message));
  exit(0);
}

// Produce a license key
function getLicenseKey($trialid, $name, $email){
  $escname = escapeshellarg($name);
  $escemail = escapeshellarg($email);
  
  exec("/home/simatrat/db/trialLicense $trialid $escname $escemail", $lines);
  
  $licenseKey = "";
  foreach($lines as $line){
    $licenseKey = $licenseKey . $line . "\n";
  }
  return $licenseKey;
}

// Email a license key to user
function mailLicenseKey($trialid, $name, $email){
  $licenseKey = getLicenseKey($trialid, $name, $email);

  $body = "The six lines below are the trial license key for simEngine.  " .
    "You may copy and paste them into your Matlab session during installation, ". 
    "or by running simCheckLicense('-update') from within Matlab.\n\n" .
    $licenseKey;

  if(mail($email,"simEngine Trial License Key", $body, "From: \"Simatra Modeling Technologies\" <support@simatratechnologies.com>")){
    header("Location: thank-you-for-trying-simEngine.html");
  }
  else{
    logError("There was an error delivering your trial license.");
  }
}
?>