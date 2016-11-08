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
$cmd = "$dir/while";

// プログラムを保存する
$prog_text = convertEOL(filter_input(INPUT_POST, "prog", FILTER_UNSAFE_RAW));
$prog_hash = substr(sha1($prog_text), 0, 8);
$res = file_put_contents("$dir/programs/$prog_hash.while", $prog_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}
$cmd .= " $dir/programs/$prog_hash.while";

// データを保存する
$data_text = convertEOL(filter_input(INPUT_POST, "data", FILTER_UNSAFE_RAW));
$data_hash = substr(sha1($data_text), 0, 8);
$res = file_put_contents("$dir/data/$data_hash.while", $data_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}
$cmd .= " $dir/data/$data_hash.while";

$cwd = "/tmp";
$descriptorspec = array(
    0 => array("pipe", "r"),
    1 => array("pipe", "w")
);
$env = array();

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
