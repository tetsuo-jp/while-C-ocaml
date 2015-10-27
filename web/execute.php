<?php

function convertEOL($string, $to = "\n")
{
    return strtr($string, array(
        "\r\n" => $to,
        "\r" => $to,
        "\n" => $to,
    ));
}

$dir = dirname(__FILE__);
$cmd = "$dir/ri";

// 引数の設定
$invert = filter_input(INPUT_POST, "invert", FILTER_VALIDATE_BOOLEAN);
$p2d =    filter_input(INPUT_POST, "p2d",    FILTER_VALIDATE_BOOLEAN);
$exp =    filter_input(INPUT_POST, "exp",    FILTER_VALIDATE_BOOLEAN);
$ri_flags = array();
if ($invert) { $cmd .= " -inverse"; }
if ($p2d)    { $cmd .= " -p2d"; }
if ($exp)    { $cmd .= " -exp"; }

// プログラムを保存する
$prog_text = convertEOL(filter_input(INPUT_POST, "prog", FILTER_UNSAFE_RAW));
$prog_hash = substr(sha1($prog_text), 0, 8);
$res = file_put_contents("$dir/programs/$prog_hash.rwhile", $prog_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}
$cmd .= " $dir/programs/$prog_hash.rwhile";

// データを保存する
$data_text = convertEOL(filter_input(INPUT_POST, "data", FILTER_UNSAFE_RAW));
$data_hash = substr(sha1($data_text), 0, 8);
$res = file_put_contents("$dir/data/$data_hash.rwhile", $data_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}
if (!($invert || $p2d || $exp)) {
	$cmd .= " $dir/data/$data_hash.rwhile";
}

$cwd = "/tmp";
$descriptorspec = array(
    0 => array("pipe", "r"),
    1 => array("pipe", "w")
);
$env = array();

// echo $cmd . "\n";
$process = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);

if (is_resource($process)) {

    fwrite($pipes[0], $prog_text);
    fclose($pipes[0]);

    $output = stream_get_contents($pipes[1]);
    fclose($pipes[1]);

    $return_value = proc_close($process);

    // echo $return_value . "\n";

    if ($return_value === 124) {
      echo "Execution timed out!\n";
    }
?>
<textarea name="output" rows="30" cols="100">
<?php
    echo $output;
?>
</textarea>
<?php
}

?>
