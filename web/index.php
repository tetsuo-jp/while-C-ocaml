<?php
$example = filter_input(INPUT_GET, "example", FILTER_VALIDATE_INT);
?>

<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>R-WHILE Playground</title>
  </head>

  <body>
  <h1>R-WHILE Playground</h1>

<form action="execute.php" method="post">
<h3>R-WHILE code</h3>
<textarea name="prog" rows="20" cols="100">
<?php
if ($example == 1) {
	$filename = "swap.rwhile";
} else if ($example == 2) {
	$filename = "piorder.rwhile";
} else if ($example == 3) {
	$filename = "ri.rwhile";
} else if ($example == 4) {
	$filename = "ri.rwhile";
} else if ($example == 5) {
	$filename = "ri.rwhile";
} else if ($example == 6) {
	$filename = "ri.rwhile";
} else {
	$filename = "reverse.rwhile";
}
$con = file_get_contents("examples/$filename");
echo($con);
 ?>
</textarea>
<h3>Input data</h3>
<textarea name="data" rows="10" cols="100">
<?php
if ($example == 1) {
    $data = "list123.val";
} else if ($example == 2) {
    $data = "piorder_input05.val";
} else if ($example == 3) {
    $data = "id_and_nil.p_val";
} else if ($example == 4) {
    $data = "reverse_and_list123.p_val";
} else if ($example == 5) {
    $data = "piorder.p_val";
} else if ($example == 6) {
    $data = "ri_ri_reverse_list123.p_val";
} else {
    $data = "list123.val";
}
$con = file_get_contents("examples/$data");
echo($con);
?>
</textarea>
<h3>Options</h3>
<input type="checkbox" name="invert" value="1">Inversion
<input type="checkbox" name="p2d" value="1">Program2data
<input type="checkbox" name="exp" value="1">Expand macros
<h3>Execute</h3>
<input type="submit" value="Execute">
</form>

<h2>Sample programs and data</h2>
<ul>
 <li> <a href="index.php?example=0">reverse</a>
 <li> <a href="index.php?example=1">swap</a>
 <li> <a href="index.php?example=2">translation from a tree to its preorder and inorder traversal (piorder)</a>
 <li> <a href="index.php?example=3">self-interpretation of an identity function</a>
 <li> <a href="index.php?example=4">self-interpretation of reverse</a>
 <li> <a href="index.php?example=5">self-interpretation of piorder</a>
 <li> <a href="index.php?example=6">self-interpretation of self-interpretation of reverse (This will probably time out in this playground.)</a>
</ul>

  </body>
</html>
