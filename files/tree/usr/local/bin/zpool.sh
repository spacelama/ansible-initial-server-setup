#!/bin/bash

# https://lars.timmann.de/wiki/index.php/ZFS_on_Linux

#
## print_zpool.sh
#

# Written by Lars Timmann <L@rs.Timmann.de> 2022

columns=5 # number of columns for zpool status
if [ ${#} -gt 0 ] && [ ${1} == "iostat" ]
then
  command="iostat -v"
  columns=7
  shift
fi

stdbuf --output=L zpool ${command:-status} -P ${*} | awk -v columns=${columns} '
BEGIN {
  command="lsscsi --scsi_id";
  while( command | getline lsscsi ) {
    count=split(lsscsi,fields);
    dev=fields[count-1];
    scsi_id[dev]=fields[1];
  }
  close(command);
  
  command="ls -Ul /dev/disk/by-id/*";
  while( command | getline ) {
    dev=$NF;
    gsub(/[\.\/]/,"",dev);
    dev_id=$(NF-2);
    device[dev_id]="/dev/"dev;
  }
  close(command);
}
$1 ~ /\/dev\// {
  line=$0;
  dev_by_id=$1;
  dev_no_part=dev_by_id;
  gsub(/(-part|)[0-9]+$/,"",dev_no_part);
  if( NF > 5) {
    count=split(line,a,FS,seps);
    line=seps[0];
    for(i=1;i<columns;i++){
      line=line a[i] seps[i];
    }
    line=line a[columns];
    for(i=columns+1;i<=count;i++){
      rest=rest a[i] seps[i];
    }
  }
  printf("%s %s %s",line,scsi_id[device[dev_no_part]],device[dev_by_id]);
  if(rest!=""){
    printf(" %s",rest);
    rest="";
  }
  printf("\n");
  next;
}
/^errors:/ {
  print;
  fflush();
  next;
}
{ 
  print;
}'
