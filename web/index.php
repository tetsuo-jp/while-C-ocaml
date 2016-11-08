<?php
$example = filter_input(INPUT_GET, "example", FILTER_VALIDATE_INT);
?>

<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>WHILE Playground</title>
  </head>

  <body>
  <h1>WHILE Playground</h1>

<form action="execute.php" method="post">
<h3>WHILE code</h3>
<textarea name="prog" rows="20" cols="100">
<?php
if ($example == 1) {
	$filename = "succ.while";
} else if ($example == 2) {
	$filename = "pred.while";
} else {
	$filename = "reverse.while";
}
$con = file_get_contents("examples/$filename");
echo($con);
 ?>
</textarea>
<h3>Input data</h3>
<textarea name="data" rows="10" cols="100">
<?php
if ($example == 1) {
    $data = "three.val";
} else if ($example == 2) {
    $data = "three.val";
} else {
    $data = "list123.val";
}
$con = file_get_contents("examples/$data");
echo($con);
?>
</textarea>

<h3>Execute</h3>
<input type="submit" value="Execute">
</form>

<h2>Sample programs and data</h2>
<ul>
 <li> <a href="index.php?example=0">reverse</a>
 <li> <a href="index.php?example=1">succ</a>
 <li> <a href="index.php?example=2">pred</a>
</ul>

  </body>
</html>
