BEGIN {count=1; getline last}
{
    if ($0 == last) {
      count++
    }
    else {
      if (count >0) {
        printf "%4d: %s\n", count, last
      }
      count=1
      last=$0
    }
}
END {
      if (count >0) {
        printf "%4d: %s\n", count, last
      }
}
