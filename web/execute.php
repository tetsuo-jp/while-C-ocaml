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

$max_input   = 65536;    // 入力サイズ上限 (64 KiB)
$max_output  = 1048576;  // 出力サイズ上限 (1 MiB)
$timeout_sec = "5";      // 実行時間上限 (秒)

// プログラムを保存する
$prog_text = filter_input(INPUT_POST, "prog", FILTER_UNSAFE_RAW);
if (!is_string($prog_text) || strlen($prog_text) > $max_input) {
    header("HTTP/1.1 413 Payload Too Large");
    exit;
}
$prog_text = convertEOL($prog_text);
$prog_hash = substr(sha1($prog_text), 0, 8);
$prog_file = "$dir/programs/$prog_hash.while";
$res = file_put_contents($prog_file, $prog_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}

// データを保存する
$data_text = filter_input(INPUT_POST, "data", FILTER_UNSAFE_RAW);
if (!is_string($data_text) || strlen($data_text) > $max_input) {
    header("HTTP/1.1 413 Payload Too Large");
    exit;
}
$data_text = convertEOL($data_text);
$data_hash = substr(sha1($data_text), 0, 8);
$data_file = "$dir/data/$data_hash.while";
$res = file_put_contents($data_file, $data_text);
if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
}

$cwd = "/tmp";
$descriptorspec = array(
    0 => array("pipe", "r"),
    1 => array("pipe", "w")
);
$env = array();

// 配列形式でシェルを経由せず、timeout で実行時間を制限して実行する (PHP 7.4+)
// 引数は固定文字列と sha1 由来の hex ファイル名のみ (ユーザ入力は渡らない)
// nosemgrep: php.lang.security.exec-use.exec-use
$process = proc_open(
    array("timeout", "-k", "1", $timeout_sec, "$dir/while", $prog_file, $data_file),
    $descriptorspec, $pipes, $cwd, $env);

if (is_resource($process)) {

    fclose($pipes[0]);

    $output = stream_get_contents($pipes[1], $max_output);
    fclose($pipes[1]);

    $return_value = proc_close($process);

    if ($return_value === 124) {
      echo "Execution timed out!\n";
    }
?>
<textarea name="output" rows="30" cols="100">
<?php
    echo htmlspecialchars($output, ENT_QUOTES, 'UTF-8');
?>
</textarea>
<?php
}

?>
