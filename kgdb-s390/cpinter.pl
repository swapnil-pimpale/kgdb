#!/usr/bin/perl

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# Copyright (C) 2002 Ganesh Varadarajan <ganesh@veritas.com>

# version 0.2.0

# this is the "backend" of the stub, which talks to CP.

$rows = -1;			# of rows in x3270 screen
$columns = -1;			# of columns
$vmstate = "";			# status line
$inputline = "";		# input field
$watchpt_id = 100;		# counter of last id used for tracepoints
$dummy_threadid = 65536;	# fake thread ids will be > 64k.
$ncpus = 0;			# no. of cpus

$|=1;
$timeout = 5;			# waiting for CP to respond

$inf = STDIN;			# file descriptors to talk to x3270
$of = STDOUT;

%memcache = ();			# memory cache
$regcache = "";			# register cache
%cpuinfo = ();			# per-cpu register cache
%threadlist = ();		# list holding all threads and their regs.
$usethread = 0;			# current thread to use for "get regs"
$current_cpu = "";

# constants

$firstpage = 4096;		# memory access below this address forbidden

require 'cpstrings.pl';
if (!$cpstring || !$holdstring || !$morestring || !$vmstring) {
	gdbstub_die("Please edit cpstrings.pl - entries missing");
}

# get a snapshot of the screen

sub screen_get
{
	my($screen);

	cmd("Snap");
	$screen = cmd("Snap(Ascii)");
	return $screen;
}

# update various globals based on the most recent screenshot.

sub status_update
{
	my($screen) = shift(@_);
	my(@lines, @status);

	@lines = split(/\n/, $screen);
	@status = split(' ', $lines[$#lines]);
	if ($status[3] eq "N") {
		print STDERR "x3270 not connected to host\n";
		gdbstub_die();
	}
	if ($#lines > 2) {
		$vmstate = join(' ', split(' ', $lines[$#lines - 1]));
		print STDERR "vmstate $vmstate \n";
		$inputline = $lines[$#lines - 2];
	}
	$rows = $status[6];
	$columns = $status[7];
	if ($rows < 25 || $columns < 80) {
		print STDERR "x3270 screen too small ($columns x $rows),",
				" need at least 80x25\n";
		gdbstub_die();
	}
}

# sends a command to x3270 and waits for status.

sub cmd
{
	my($cmdline) = shift(@_);
	my($input, $line);

	print STDERR "cmd: $cmdline\n";
	print $of "$cmdline\n";
	while ($input = <$inf>) {
		if ($input eq "error\n") {
			if ($cmdline =~ /Wait/) {
				return "";
			}
			print STDERR "cmd:error:$line\n"; 
			gdbstub_die();
		} elsif ($input eq "ok\n") {
			$line =~ s/^data: //gm;
			status_update($line);
			return $line;
		}
		$line .= $input;
	}
}

# send a CP command.

sub cmd_string
{
	cmd("String \"$_[0]\"");
	cmd("Enter");
}

# enter CP mode.

sub cp_enter
{
	screen_get();
	if ($vmstate =~ /$cpstring/i) {
		print STDERR "cp_enter: already in CP\n";
		return;
	}
	print STDERR "cp_enter_slow: $vmstate\n";
	cmd("PA 1");
	cp_wait();
	return;
}

# wait until the machine actually goes to CP mode, possibly after executing
# a command.  we use a somewhat questionable strategy here - we wait for the
# status to go to "CP Read" and the input line to be emptied. in practice
# this has been observed to be necessary and sufficient.
#
# the right way to do this is to issue a dummy command like foo$count (and keep
# incrementing $count so that we issue unique commands each time) after the
# real command and wait for the CP error HCPCMD... unknown command foo24.
# this will guarantee that the previous command has been completed. however,
# latency is our biggest problem right now and I don't want to add to it
# unless the existing hack fails for some situation. which might very well
# happen if the stub and the target reside on the same LAN.

sub cp_wait()
{
	my($screen);
	local $SIG{ALRM} = sub { print STDERR "Timed out waiting for CP ($vmstate ##$inputline##, retrying\n"; alarm($timeout)};

	alarm($timeout);
	while (1) {
		$screen = screen_get();
		print STDERR "cp_wait: $vmstate\n";
		if ($vmstate =~ /$cpstring/i && $inputline =~ /^\s*$/) {
			alarm(0);
			return;
		} elsif ($vmstate =~ /$holdstring/i || $vmstate =~ /$morestring/i) {
			cmd("Clear");
		}
		cmd("Wait(1,Output)");
	}
}

# leave CP and start the VM from where it left off.

sub cp_leave
{
	my($screen);
	local $SIG{ALRM} = sub { print STDERR "Timed out waiting for VM ($vmstate ##$inputline##)\n"; };

	# of course we have to trash the memory cache.
	%memcache = ();
	$regcache = "";
	%cpuinfo = ();
	%threadlist = ();
	$usethread = 0;
	$current_cpu = "";

	cp_wait();

	screen_get();
	if ($vmstate !~ /$cpstring/i) {
		print STDERR "cp_leave: Not in CP mode ($vmstate)\n";
		return;
	}

	# PA1 is a toggle.

	cmd("PA 1");

	# wait for it to actually exit CP. otherwise we'll have a false
	# detection of the machine breaking into CP.

	alarm($timeout);
	while (1) {
		$screen = screen_get();
		print STDERR "cp_wait: $vmstate\n";
		if ($vmstate =~ /$holdstring/i || $vmstate =~ /$morestring/i) {
			cmd("Clear");
		} elsif ($vmstate !~ /$cpstring/i) {
			alarm(0);
			return;
		}
		cmd("Wait(1,Output)");
	}
}

# get all registers. ignore and ECR FP regs for now. they shouldn't
# need to be modified from the gdb side and if you really want to
# see them use the console.

sub register_get
{
	my($registers, $screen, $i);

	if ($usethread) {
		return lc($threadlist{$usethread}{"regs"});
	}

	if ($regcache ne "") {
		return lc($regcache);
	}

	cmd("Clear");
	cmd_string("d psw g");
	cp_wait();
	$screen = screen_get();

	if ($screen !~ /PSW = ([[:xdigit:]]+) ([[:xdigit:]]+)/mi) {
		return "PSW error";
	}
	$registers .= $1 . $2;
	
	for ($i = 0; $i <= 12; $i += 4) {
		if ($screen !~ /GPR\s+$i =  ([[:xdigit:]]+)  ([[:xdigit:]]+)  ([[:xdigit:]]+)  ([[:xdigit:]]+)/mi) {
			return "GPR$i error";
		}
		$registers .= $1 . $2 . $3 . $4;
	}

	$registers .= "0" x (8 * 16);		# ECR regs
	$registers .= "00000000";		# FPC
	$registers .= "0" x (16 * 16);		# FP regs

	return $regcache = lc($registers);
}

# set all registers. gdb screws up while setting the PSW, so we need to set
# the MSB ourselves.
# need to be careful not to overflow input line.
# we don't set CR and FP regs for now.

sub register_set
{
	my($gdbregisters) = shift(@_);
	my(@registers, $cpcmd, $screen, $count);

	if ($usethread) {
		return "error";
	}

	%memcache = ();
	$regcache = "";

	@registers = unpack("a8" x 34, $gdbregisters);
	cmd("Clear");
	$cpcmd = "st psw " . shift(@registers) . " ";
	$cpcmd .=  sprintf("%08x", hex(shift(@registers)) | 0x80000000);
	
	cmd_string($cpcmd);
	for ($i = 0; $i < 16; $i += 8) {
		$cpcmd = "st g$i " . join(' ', @registers[$i .. $i + 7]);
		cmd_string($cpcmd);
	}
	cp_wait();
	$screen = screen_get();
	$count = 0;
	$screen =~ s/Store complete/$count++/egmi;
	if ($count != 3) {
		print STDERR "Store error $count\n";
		return "Store error";
	}
	$regcache = $gdbregisters;
	return "OK";
}

# gdb does a pretty awful job (if at all it tries) of caching
# memory from the target. you can often see it reading the same
# 2 bytes it read just a packet ago. obviously this does wonders
# for response times. it can take several coffees before a backtrace
# emerges from the rubble.
# 
# so we cache memory on the stub, taking care to see that any store
# to memory, registers or cp_leave() will invalidate the cache. of course
# we assume that all CPUs are stopped and no external agent can modify
# memory when the machine is in CP mode. if you modify memory using x3270
# behind the stub's back, there will be trouble ...
#
# there are actually 2 caches. the primary cache is organized as
# "cache lines" of 256 bytes aligned at 256-byte boundaries. any
# address:length pair which falls completely within a cache line is
# satisfied from this cache. odd address:length pairs which straddle
# cache lines are looked up in the secondary cache which is indexed simply by
# address:length.
# if you think this is overly elaborate and unnecessary, your stub and target
# probably reside on the same LAN :)

sub memory_get
{
	my($address, $length, @rest) = @_;
	my($cline, $offset);

	# shield unwary user from dereferencing NULL pointers.
	# wary users can either comment this or implement a specific
	# stub command to toggle this behaviour.

	if (hex($address) < $firstpage) {
		return "error";
	}
		
	if (exists($memcache{"$address:$length"})) {
		return $memcache{"$address:$length"};
	}

	$cline = sprintf("%08X", hex($address));
	$offset = substr($cline, 6, 2, "");
	if (hex($length) <= 256 - hex($offset)) {
		if (exists($memcache{$cline})) {
			if ($memcache{$cline} =~ /error/) {
				return $memcache{$cline};
			}
			return substr($memcache{$cline}, hex($offset) * 2, hex($length) * 2);
		}
		$memcache{$cline} = memory_get_cp($cline . "00", "100");
		if ($memcache{$cline} =~ /error/) {
			return $memcache{$cline};
		}
		return substr($memcache{$cline}, hex($offset) * 2, hex($length) * 2);
	}
			
	return $memcache{"$address:$length"} = memory_get_cp($address, $length);
}


# actually get memory contents from CP. again we must be careful to avoid
# screen overruns.

sub memory_get_cp
{
	my($address, $length, @rest) = @_;
	my($screen, @lines, @words, $dump, $last);

	if (hex($length) > ($rows - 3) * 16) {
		print STDERR "memory_get: too much data $address:$length\n";
		return "error";
	}

	cmd("Clear");
	cmd_string("d $address.$length");
	cp_wait();

	$last = "error";
	$screen = screen_get();
	if ($screen =~ /nonvalid|non-addressable|exception/i) {
		return $last;
	}

	$screen =~ s/^[[:xdigit:]]{2}: //gm;
	@lines = split(/\n/, $screen);

	for ($i = 1; $i < $#lines; $i++) {
		if ($lines[$i] =~ /^V([[:xdigit:]]+)/i) {
			@words = split(' ', $lines[$i]);
			if ($words[1] eq "to") {
				$dump .= $last x ((hex($words[2]) - hex($1) + 1) / 16);
			} else {
				$last = join('', @words[1 .. (($#words < 4) ? $#words : 4)]);
				$dump .= $last;
			}
		}
	}

	return lc(substr($dump, 0, hex($length) * 2));
}

# set memory. be careful not to overrun the input line.

sub memory_set
{
	my($address, $len, $data, @rest) = @_;
	my($i, $haddr, $chunk, $screen);
	my($count) = 0;
	my($maxinputline) = int($columns * 3 / 2);

	# protect the unwary from writing to per-cpu data.

	$haddr = hex($address);
	if ($haddr < $firstpage) {
		return "error";
	}

	%memcache = ();

	cmd("Clear");
	for ($i = $haddr; $i < $haddr + $len;) {
		$cpcmd = "st s" . sprintf("%X", $i) . " ";
		$chunk = substr($data, 0, $maxinputline, "");
		$cpcmd .= $chunk;
		$i += length($chunk);
		cmd_string($cpcmd);
		$count++;
	}
	cp_wait();
	$screen = screen_get();
	$screen =~ s/Store complete/$count--/egmi;
	if ($count != 0) {
		print STDERR "Store error $count\n";
		return "Store error";
	}
	return "OK";
}

# set breakpoints and store watchpoints. also handle single step
# watchpoints. the types are defined by the gdb remote protocol.
# 55 is our special code for single step.

sub watchpoint_set
{
	my($address, $length, $type, @rest) = @_;
	my($wid, $cpcmd, $screen);

	$wid = sprintf("%X", hex($address)) . ":$type";
	if (exists($watchpoint_id{$wid})) {
		cmd_string("cpu all tr del $watchpoint_id{$wid}");
		delete($watchpoint_id{$wid});
	}
	if ($type == 0 || $type == 1) {
		$cpcmd = "cpu all tr i r pri $address.$length id $watchpt_id";
	} elsif ($type == 2) {
		$cpcmd = "cpu all tr st into pri $address.$length id $watchpt_id";
	} elsif ($type == 55) {
		$cpcmd = "tr i id $watchpt_id";
	} elsif ($type == 56) {
		$cpcmd = "tr i r pri $address.$length id $watchpt_id";
	} else {
		return "error";
	}
	cmd_string($cpcmd);
	cp_wait();
	$screen = screen_get();
	if ($screen =~ /HCPTR/mgi) {
		return "error setting trace";
	}
	$watchpoint_id{$wid} = $watchpt_id++;
	return "OK";
}

sub watchpoint_del
{
	my($address, $length, $type, @rest) = @_;
	my($wid, $cpcmd);

	$wid = sprintf("%X", hex($address)) . ":$type";
	if (exists($watchpoint_id{$wid})) {
		if ($type > 55) {
			cmd_string("tr del $watchpoint_id{$wid}");
		} else {
			cmd_string("cpu all tr del $watchpoint_id{$wid}");
		}
		delete($watchpoint_id{$wid});
		return "OK";
	} else {
		return "error: watchpoint not found";
	}
}

# resume the VM, possibly in single step mode.
# XXX - gdb might screw up setting the PSW address here too, check.

sub resume
{
	my($addr, $step, @rest) = @_;
	my($screen);

	if ($addr ne "current") {
		cmd("Clear");
		cmd_string("d psw");
		cp_wait();
		$screen = screen_get();
		if ($screen !~ /PSW = ([[:xdigit:]]+) ([[:xdigit:]]+)/mi) {
			return "PSW error";
		}
		$addr = sprintf("%08x", hex($addr) | 0x80000000);
		cmd_string("st psw $1 $addr");
		cp_wait();
		$screen = screen_get();
		if ($screen !~ /Store complete/mgi) {
			print STDERR $screen;
			return "PSW store error";
		}
	}
	if ($step) {
		single_step_begin();
	}
	cmd("Clear");
	return "OK";
}

$step_point = "";

sub step_over_range
{
	my($start, $stop, @rest) = @_;
	my($w);
	
	$w = watchpoint_set($stop, "2", 56);
	if ($w =~ /error/) {
		print STDERR "step_over_range: $w\n";
		return $w;
	}
	$step_point = $stop;

	cmd("Clear");
	return "OK";
}

sub step_over_range_end
{
	my($w);

	if ($step_point) {
		$w = watchpoint_del($step_point, "2", 56);
		$step_point = "";
	}
}

# check if the machine has dropped into CP , either because of a
# breakpoint or a CP exception.
#
# if the last printed line on the screen contains an address
# then we presume it's a breakpoint. otherwise we tell gdb that
# it's a segfault.
#
# sometimes x3270 displays "VM Read" or "Running" even though the
# machine has dropped into CP. if we suspect that the machine is indeed
# in CP (the last printed line contains an address or an HCP message), we
# gently nudge it with an Enter. this is usually sufficient to update
# the status field. in case this doesn't work, manually push it into
# CP using PA1 or #cp on the x3270 console - the stub will catch on
# immediately.

sub break_check
{
	my($screen, @lines, $i);

	$screen = screen_get();
	if ($vmstate =~ /$holdstring|$morestring/i) {
		cmd("Clear");
		return 0;
	}

	@lines = split(/\n/, $screen);
	for ($i = $#lines - 3; $i >= 0; $i--) {
		last if ($lines[$i] !~ /^\s*$/);
	}

	if ($vmstate =~ /$cpstring/i) {
		if ($i < 0 || $lines[$i] !~ /[[:xdigit:]]{8}\' /) {
			return $signo{SEGV};
		}
		if ($lines[$i] =~ /^([[:xdigit:]]{2}): .*[[:xdigit:]]{8}\' /) {
			$current_cpu = $1;
			cmd_string("cpu $1");
		}
		return $signo{TRAP};
	}
	if ($lines[$i] =~ /([[:xdigit:]]{8}\' )|HCP/i) {
		cmd("Enter");
	}
	return 0;
}

sub single_step_end
{
	watchpoint_del(0, 0, 55);
}

# initiate single stepping. watchpoint_set() treats 0,0,55 as a
# special case. 

sub single_step_begin
{
	watchpoint_set(0, 0, 55);
}

sub step_end
{
	single_step_end();
	step_over_range_end();
}

# at the beginning and ending of the session, we need to drain the
# fifo of any x3270 responses, so that the simple synchronous
# command/response protocol of communication with x3270 does not get
# confused. this is both to protect ourselves from responses to
# past stubs and to clean up our mess for future stubs.
#
# there's no way this can be done exactly right, so waiting for 2
# seconds is good enough.

sub input_drain
{
	eval {
		local $SIG{ALRM} = sub { die "" };
		alarm(2);
		while (<>) {};
	};
}

$threadid_last_pkt = -1;
$thread_max = 15;

sub threadinfo
{
	my($i, $nthreads, $reply);

	if (!%threadlist) {
		cp_threadinfo();
	}

	$nthreads = 0;
	$reply = "";
	for $i (sort { $a <=> $b } keys(%threadlist)) {
		last if $nthreads >= $thread_max;
		next unless $i > $threadid_last_pkt;
		if ($reply ne "") {
			$reply .= ",";
		}
		$reply .= sprintf("%08x", $i);
		$nthreads++;
		$threadid_last_pkt = $i;
	}
	if ($reply eq "") {
		$reply = "l";
		$threadid_last_pkt = -1;
	} else {
		$reply = "m$reply";
	}
	return $reply;
}

# get registers on all CPUs. this is required because threads running
# on CPUs will have junk in their struct thread.

sub cpuinfo_get
{
	my($screen, $i, $cpcmd);

	if (!$current_cpu) {
		cmd("Clear");
		cmd_string("q cpu");
		$screen = screen_get();
		if ($screen !~ /^([[:xdigit:]]{2}): CPUID/mi) {
			print STDERR "couldn't figure out current cpu\n";
			return "";
		}
		$current_cpu = $1;
	}
	for $i (keys(%cpuinfo)) {
		if ($i == hex($current_cpu) && !$usethread) {
			$cpuinfo{$i} = register_get();
			next;
		}
		cmd("Clear");
		cmd_string("cpu " . sprintf("%02x", $i) . " cmd d psw g");
		cp_wait();
		$screen = screen_get();
		if ($screen !~ /PSW = ([[:xdigit:]]+) ([[:xdigit:]]+)/mi) {
			return "PSW error";
		}
		$cpuinfo{$i} = $1 . $2;
	
		for ($j = 0; $j <= 12; $j += 4) {
			if ($screen !~ /GPR\s+$j =  ([[:xdigit:]]+)  ([[:xdigit:]]+)  ([[:xdigit:]]+)  ([[:xdigit:]]+)/mi) {
				return "GPR$j error";
			}
			$cpuinfo{$i} .= $1 . $2 . $3 . $4;
		}
	}
	return "OK";
}

# getting threadinfo out of a machine requires the usage of a helper
# module called gdbmod. we carefully call a function in the virtual
# machine to gather threadinfo for all the threads and store it in a
# contiguous buffer. this greatly reduces the overhead of collecting thread
# data and makes info threads usable when the target is way across the net.
#
# alternate implementations which don't involve a helper module are possible
# by walking the task list from the stub given the address of the init task
# from ksyms. it may even take less than a lifetime if the target is on the
# LAN, I don't know.

sub cp_threadinfo
{
	my($chunksize, $oldregs, $cpcmd, $screen, $count, $cookie, $recsize);
	my($nthreads, @tinfo, $addr, $i, $regs);

	$recsize = 64;		# magic number, size of threadinfo structure

	if (!$gdbmod_dispatch || !$gdbmod_stack || !$gdbmod_return || !$gdbmod_data) {
		gdbmod_addrs_get();
		if (!$gdbmod_dispatch || !$gdbmod_stack || !$gdbmod_return || !$gdbmod_data) {
			print STDERR "gdbmod addresses not available\n";
			return "";
		}
	}

	if (cpuinfo_get() ne "OK") {
		print STDERR "couldn't get cpuinfo\n";
		return "";
	}

	# chunksize is the unit in which we retrieve thread info. it's the size of
	# a screenful of data.

	$chunksize = ($rows - 3) * 16;
	$chunksize = int($chunksize / $recsize) * $recsize;
	print STDERR "chunksize $chunksize\n";

	# now we save the registers of the current CPU and place a breakpoint
	# on gdbmod_return(). then we proceed to set the registers as follows:
	# r15	= private stack for gdbmod functions.
	# r2	= function code (1 = get threadinfo)
	# r3	= a cookie which must be set in the data buffer to ensure that
	#		gdbmod actually worked.
	# psw	= the state is set to disable all interrupts/supervisor mode/
	#	  primary address space. PC is set to gdbmod_dispatch().
	#
	# the stack is arranged so that there's some space for gdbmod_dispatch()
	# to save the state of its phantom "caller". then the current cpu is
	# started.
	# when done, gdbmod_dispatch() will call gdbmod_return(), on which we've
	# placed a breakpoint, conveniently returning control to us. then we can
	# snarf the data collected in the gdbmod_data buffer.
	# note that we only start the current CPU - others remain in the stopped
	# state.
	#
	# this is the easiest way I could think of to get a function run on the
	# guest without all the tedious stuff which goes on in SAVE_ALL.
	#
	# see gdbmod.c for an idea of what the data we retrieve looks like.

	$oldregs = register_get();
	if (watchpoint_set($gdbmod_return, "4", 0) ne "OK") {
		print STDERR "unable to set return breakpoint\n";
		return "";
	}
	cmd_string("st psw 04080000 $gdbmod_dispatch");
	$cookie = sprintf("%08x", time());
	$cpcmd = "st g2 1 g3 " . $cookie;
	$cpcmd .= " g15 $gdbmod_stack g11 $gdbmod_stack";
	cmd_string($cpcmd);
	$screen = screen_get();
	$count = 0;
	$screen =~ s/Store complete/$count++/egmi;
	if ($count != 2) {
		print STDERR "store error, attempting to restore...";
		print STDERR register_set($oldregs), "\n";
		return "";
	}
	cmd_string("b cpu $current_cpu");
	do {
		cp_wait();
	} while (break_check() != $signo{TRAP});

	if (register_set($oldregs) ne "OK") {
		print STDERR "restore error - your VM is probably fubared\n";
		gdbstub_die();
	}
	if (watchpoint_del($gdbmod_return, "4", 0) ne "OK") {
		print STDERR "unable to del return breakpoint\n";
	}

	# the first record contains the number of subsequent records to be
	# retrieved.

	if (($mem = memory_get_cp($gdbmod_data, sprintf("%08x", $recsize))) =~ /error/) {
		print STDERR "error retrieving gdbmod data\n";
		return "";
	}
	@tinfo = unpack("a8a32a8", $mem);
	if (hex($tinfo[2]) != hex($cookie)) {
		print STDERR "cookie mismatch! wanted $cookie got $tinfo[2]\n";
		return "";
	}
	$nthreads = hex($tinfo[0]);
	$addr = sprintf("%08x", hex($gdbmod_data) + $recsize);
	while ($nthreads) {
		$toread = ($nthreads * $recsize < $chunksize) ? $nthreads * $recsize : $chunksize;
		print STDERR "nthreads $nthreads $toread\n";
		if (($mem = memory_get_cp($addr, sprintf("%08x", $toread))) =~ /error/) {
			print STDERR "error retrieving gdbmod data\n";
			return "";
		}
		$addr = sprintf("%08x", hex($addr) + $toread);
		while ($mem) {
			@tinfo = unpack("a8a32a8a80", substr($mem, 0, $recsize * 2, ""));
			if (hex($tinfo[2]) & 0x80000000) {

				# this thread is running on a cpu.

				$regs = $cpuinfo{hex($tinfo[2]) & 0x7fffffff};
				$regs .= "00000000" x 16;
			} else {
				$regs = "04080000" . substr($tinfo[3], 8 * 8, 8);
				$regs .= "00000000" x 6 . $tinfo[3];
				$regs .= "00000000" x 16;
			}
			$threadlist{hex($tinfo[0])} = {
				name 	=> $tinfo[1],
				regs	=> $regs,
			};
			$nthreads--;
		}
	}
}

sub cp_init
{
	my($screen, @lines);

	# required so that the machine stays in CP mode after executing a
	# CP command.

	cmd_string("set run off");
	cmd("Clear");
	cmd_string("q v cpus");
	cp_wait();
	$screen = screen_get();
	@lines = split(/\n/, $screen);

	for ($i = 1; $i < $#lines; $i++) {
		if ($lines[$i] =~ /CPU ([[:xdigit:]]{2})/i) {
			$cpuinfo{hex($1)} = "";
			$ncpus++;
		}
	}

}

sub thread_switch
{
	my($packet) = shift(@_);

	if ($packet eq "0") {
		$usethread = 0;
		return "OK";
	}
	if (!exists($threadlist{hex($packet)})) {
		return "error: nonexistent thread";
	}
	$usethread = hex($packet);
	return "OK";
}

sub thread_alive
{
	my($packet) = shift(@_);

	if (exists($threadlist{hex($packet)})) {
		return "OK";
	}
	return "error";
}

sub thread_extrainfo
{
	my($packet) = shift(@_);

	if (exists($threadlist{hex($packet)})) {
		return $threadlist{hex($packet)}{"name"};
	}
	return "error";
}

sub addcontext
{
	my($name, $frameptr, $pc, @rest) = @_;
	my($mem, $regs);

	if (($mem = memory_get_cp($frameptr, "40")) =~ /error/) {
		return $mem;
	}
	if (!$pc) {
		$pc = substr($mem, 14 * 8, 8);
	}
	$regs = "04080000" . $pc;
	$regs .= $mem;
	$regs .= "00000000" x 16;
	$name =~ s/(.)/sprintf("%02x", ord($1))/eg;
	$threadlist{$dummy_threadid++} = {
		name	=> $name,
		regs	=> $regs,
	};
	return "OK";
}

# looks up symbol addresses in a file called ksyms in the local directory.
# this file must be obtained from the target after loading the gdbmod module.

sub gdbmod_addrs_get
{
	open(KS, "./ksyms") or return;

	while (<KS>) {
		if (/([[:xdigit:]]+)\s+gdbmod_dispatch/) {
			$gdbmod_dispatch = $1;
			$gdbmod_dispatch = sprintf("%08x", hex($gdbmod_dispatch) | 0x80000000);
		}
		if (/([[:xdigit:]]+)\s+gdbmod_stack/) {
			$gdbmod_stack = $1;
			$gdbmod_stack = sprintf("%08x", hex($gdbmod_stack) + 3072);
		}
		if (/([[:xdigit:]]+)\s+gdbmod_return/) {
			$gdbmod_return = $1;
		}
		if (/([[:xdigit:]]+)\s+gdbmod_data/) {
			$gdbmod_data = $1;
		}
	}
	close(KS);
}

# return 1 to any one 'require'ing us.
1;
