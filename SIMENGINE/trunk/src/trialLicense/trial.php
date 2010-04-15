<html>
  <head><title>simEngine Trial License Key Generator</title></head>
  <body>
    <center>
      <h2>simEngine Trial License Key Generator</h2>
    </center>
    <form action="trialLicense.php" method="POST">
      <input type="hidden" name="secret" value="simEngineTrialSecret" />
      <table width="800" align="center" bgcolor="#c0c0c0">
	<tr>
	  <th align="right">Name:</th>
	  <td align="left"><input type="text" name="customerName" /></td>
	</tr>
	<tr>
	  <th align="right">Email:</th>
	  <td align="left"><input type="text" name="customerEmail" /></td>
	</tr>
	<tr>
	  <th align="right">What best describes you?</th>
	  <td align="left">
	    <select name="usertype">
	      <option value=""></option>
	      <option value="Industry Professional">Industry Professional</option>
	      <option value="Faculty/Postdoc">Faculty/Postdoc</option>
	      <option value="Graduate student">Graduate student</option>
	      <option value="Undergraduate student">Undergraduate student</option>
	      <option value="High school student">High school student</option>
	      <option value="Hobbyist">Hobbyist</option>
	    </select>
	</tr>
	<tr>
	  <th align="right">What best describes your background?</th>
	  <td align="left">
	    <select name="background">
	      <option value=""></option>
	      <option value="Electrical Engineering">Electrical Engineering</option>
	      <option value="Mechanical Engineering">Mechanical Engineering</option>
	      <option value="Software Engineering/Computer Science">Software Engineering/Computer Science</option>
	      <option value="Other Engineering">Other Engineering</option>
	      <option value="Mathematics">Mathematics</option>
	      <option value="Physics">Physics</option>
	      <option value="Chemistry">Chemistry</option>
	      <option value="Life Sciences">Life Sciences</option>
	      <option value="Other Science">Other Science</option>
	      <option value="Finance">Finance</option>
	      <option value="Other">Other</option>
	    </select>
	</tr>
	<tr>
	  <th align="right">What best describes your anticipated use of simEngine?</th>
	  <td align="left">
	    <select name="usage">
	      <option value=""></option>
	      <option value="Commercial/Industrial">Commercial/Industrial</option>
	      <option value="Academic Research">Academic Research</option>
	      <option value="Educational Study">Educational Study</option>
	      <option value="Personal">Personal</option>
	    </select>
	</tr>
   </table>
      <center>
   <?
   require_once('recaptchalib.php');
   $publickey = "6Le7cAwAAAAAABFQUlETVx4vhOwDqfJCRDyF6ing";
   echo recaptcha_get_html($publickey, $error);
   ?>
	<input type="submit" value="Create License" />
      </center>
    </form>
  </body>
</html>