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

# this is the bit which interfaces with gdb. the remote protocol is defined
# in gdb info pages. the packet protocol is pretty horrible and things
# can go haywire quickly if resends are involved. it's best to terminate
# and restart the stub and gdb if they go out of sync.
#
# with a little work, it can be made a generic gdb stub, which requires
# an appropriate backend to talk to the native debugger. for instance,
# we could write a kadbinter.pl for solaris similar to cpinter.pl

use Socket;
use Config;

defined $Config{sig_name} || die "No sigs?";
foreach $name (split(' ', $Config{sig_name})) {
    $signo{$name} = $i;
    $i++;
}

require 'cpinter.pl';

$gdbport = 5513;		# connect from gdb using ta re hostname:5513
$socket = 0;			# socket fd

main();

# get a single byte from gdb.

sub getchar
{
	my($byte, $nread);

	$nread = sysread($socket, $byte, 1);
	if (!defined($nread)) {
		print STDERR "connection to gdb died\n";
		gdbstub_die();
	} elsif ($nread == 0) {
		print STDERR "received EOF from gdb\n";
		gdbstub_die();
	} elsif ($nread == 1) {
		return $byte;
	}
}

# despite the name, sends a string of chars to gdb.

sub putchar
{
	my($data, @rest) = @_;
	my($nwritten);

	while (length($data) > 0) {
		$nwritten = syswrite($socket, $data);
		if (!defined($nwritten) || $nwritten == 0) {
			print STDERR "connection to gdb died\n";
			gdbstub_die();
		}
		substr($data, 0, $nwritten, "");
	}
}

# gets a packet from gdb, verifies checksum and acks the packet.

sub packet_get
{
	my($packet, $data, $byte, $checksum, $xsum);

	do {
		while (getchar() ne "\$") {};
		while (($byte = getchar()) ne "#") {
			$packet .= $byte;
		}	
		$xsum = hex(getchar()) * 16;
		$xsum += hex(getchar());
		$checksum = unpack("%8C*", $packet);
		if ($xsum != $checksum) {
			putchar("-");
		} else {
			putchar("+");
			if (length($packet) >= 3 && substr($packet, 2, 1) eq ":") {
				putchar(substr($packet, 0, 2, ""));
			}
		}
	} while ($checksum != $xsum);

	return $packet;
}

# encapsulates a response in gdb's packet format, tacks on a checksum and
# resends until it gets a positive ack.

sub packet_put
{
	my($packet, @rest) = @_;
	my($checksum);

	do {
		putchar("\$$packet#" . sprintf("%02X", unpack("%8C*", $packet)));
	} while (getchar() ne "+");
}

# the main routine. is called whenever we enter CP either by hitting a
# breakpoint/exception or at the behest of gdb's 0x03 packet. we should
# already be in CP when we get here. it handles the entire conversation
# with gdb and returns when gdb asks the stub to resume or continue.
#
# currently we handle the following packets: S, g, G, m, M, c, s, Z and
# z. they are documented in gdb's info pages.

sub exception_handle
{
	my($sig, @rest) = @_;
	my($packet, $cmd, @parms);

	step_end();
	packet_put("S". sprintf("%02X", $sig));

	while (1) {
		$packet = packet_get();
		$cmd = substr($packet, 0, 1, "");
		$reply = "";

		if ($cmd eq "?") {
			$reply = "S" . sprintf("%02X", $sig);
		} elsif ($cmd eq "g") {
			$reply = register_get();
		} elsif ($cmd eq "G") {
			$reply = register_set($packet);
		} elsif ($cmd eq "m") {
			@parms = split(',', $packet);
			$reply = memory_get($parms[0], $parms[1]);
		} elsif ($cmd eq "M") {
			@parms = split(/[,:]/, $packet);
			$reply = memory_set($parms[0], $parms[1], $parms[2]);
		} elsif ($cmd eq "c") {
			if ($packet eq "") {
				$packet = "current";
			}
			$reply = resume($packet, 0);
		} elsif ($cmd eq "s") {
			if ($packet eq "") {
				$packet = "current";
			}
			$reply = resume($packet, 1);
		} elsif ($cmd eq "Z") {
			@parms = split(',', $packet);
			$reply = watchpoint_set($parms[1], $parms[2], $parms[0]);
		} elsif ($cmd eq "z") {
			@parms = split(',', $packet);
			$reply = watchpoint_del($parms[1], $parms[2], $parms[0]);
		} elsif ($cmd eq "q") {
			$reply = q_cmd($packet);
		} elsif ($cmd eq "H") {
			$reply = H_cmd($packet);
		} elsif ($cmd eq "T") {
			$reply = thread_alive($packet);
		} elsif ($cmd eq "Q") {
			$reply = Q_cmd($packet);
		} elsif ($cmd eq "e") {
			@parms = split(',', $packet);
			$reply = step_over_range($parms[0], $parms[1]);
		} else {
			print STDERR "Unknown packet:$cmd:$packet\n";
		} 
		if ($reply =~ /error/) {
			print STDERR "$cmd:$packet:$reply\n";
			$reply = "E00";
		}
		packet_put($reply);
		if (($cmd eq "c" || $cmd eq "s" || $cmd eq "e") && $reply eq "OK") {
			return;
		}
	}
}

# main select loop. it continuously polls x3270 to check for a state
# change into CP. on this event or on receiving a ^C from gdb, it
# enters CP and starts talking to gdb.

sub mainloop
{
	my($rin, $sig, $byte, $nfound, $timeleft);

	while (1) {
		$rin = '';
		vec($rin, fileno($socket), 1) = 1;
		($nfound, $timeleft) = select($rin, undef, undef, 0);
		if ($nfound) {
			if (($byte = getchar()) eq "\003") {
				cp_enter();
				exception_handle($signo{TRAP});
				cp_leave();
			} else {
				print STDERR "mainloop: discarding $byte\n";
			}
		}
		if ($sig = break_check()) {
			cp_enter();
			exception_handle($sig);
			cp_leave();
		}
		if (!$nfound) {
			select(undef, undef, undef, 0.1);
		}
	}
}

sub main
{

	# need to consume any left over output from the previous session.
	# remember we're using named pipes.

	input_drain();

	cp_enter();
	cp_init();

	my $port = $gdbport;
	my $proto = getprotobyname("tcp");
	socket(SERVER, PF_INET, SOCK_STREAM, $proto)		|| die "socket: $!";
	setsockopt(SERVER, SOL_SOCKET, SO_REUSEADDR, pack("l", 1))   || die "setsockopt: $!";
	bind(SERVER, sockaddr_in($port, INADDR_ANY))		|| die "bind: $!";
	listen(SERVER, SOMAXCONN)				|| die "listen: $!";

	my $paddr;
	$paddr = accept(CLIENT, SERVER);
	setsockopt(CLIENT, &Socket::IPPROTO_TCP, &Socket::TCP_NODELAY, 1) ||
			 die "setsockopt: $!";
	$socket = CLIENT;

	exception_handle($signo{TRAP});
	cp_leave();
	mainloop();
}

sub gdbstub_die
{
	input_drain();
	die(@_);
}

sub q_cmd
{
	my($packet) = shift(@_);
	my(@parms);

	if ($packet eq "fThreadInfo") {
		return threadinfo();
	} elsif ($packet eq "sThreadInfo") {
		return threadinfo();
	} elsif ($packet =~ /ThreadExtraInfo/) {
		@parms = split(',', $packet);
		return thread_extrainfo($parms[1]);
	}
	return "";
}

sub Q_cmd
{
	my($packet) = shift(@_);
	my(@parms);

	@parms = split(',', $packet);
	if ($parms[0] eq "a") {
		return addcontext($parms[1], $parms[2], $parms[3]);
	}
	return "";
}

sub H_cmd
{
	my($packet) = shift(@_);
	my($subcmd);

	$subcmd = substr($packet, 0, 1, "");
	if ($subcmd eq "g") {
		return thread_switch($packet);
	} elsif ($subcmd eq "c") {
		return "OK";
	}
	return "";
} 
