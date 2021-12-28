#!/usr/bin/perl -w


my$database="all gene fpkm.txt";
my$input_file="queryid.csv";
my$output_file="fpkm.txt";
if(@ARGV>2)
{
	$database=shift @ARGV;
	$input_file=shift @ARGV;
	$output_file=shift @ARGV;
}
open(FILE1,"$database")||die "open $database fail";
open(FILE2,"$input_file")||die "open $input_file fail";
open(FILE3,">>$output_file")||die "create $output_file fail";
my@id_list=();
while(my$line=<FILE2>)
{
	my$tmp=$line;
	$tmp=~s/\W+$//;
	$tmp=~s/^\W+//;
	
	if($tmp)
	{
			push(@id_list,$tmp);
			
	}
}
close(FILE2);
my$id_num=@id_list;
print "id number:$id_num\n";
#########################for large file for instance:100M-5G or more

my$tmp_head='';
my$tmp_seq='';
while(my$line=<FILE1>)
{
	my$tmp=$line;	
	if($tmp=~/^\>/)
	{		
		if($tmp_head)
		{
			for(my$i=0;$i<@id_list;$i++)
			{
				my$tmp_id=$id_list[$i];
				if($tmp_head=~/$tmp_id/i)
				{
					print FILE3 "$tmp_head\n";
					print FILE3 "$tmp_seq";					
					last;
				}
			}
		}
		$tmp_head=$tmp;
		$tmp_head=~s/\n//g;		
		$tmp_seq='';
	}
	else
	{
		if($tmp=~/\w/){$tmp_seq .=$tmp;}
	}
}
if($tmp_head)
{
			for(my$i=0;$i<@id_list;$i++)
			{
				my$tmp_id=$id_list[$i];
				if($tmp_head=~/$tmp_id/i)
				{
					print FILE3 "$tmp_head\n";
					print FILE3 "$tmp_seq";					
					last;
				}
			}
}
close(FILE1);
close(FILE3);
