#!/usr/bin/perl 
#-w

# pallike (c) copyright jaak pruulmann-vengerfeldt, 2004, 2010. all rights reserved.

use threads;
use threads::shared;
use LWP;
use XML::Parser;
use strict;
use Data::Dumper;
use Net::IRC;
use POSIX;
use Unicode::String qw{ latin1 utf8 };

use vars qw{  $ua $parser };
$ua = LWP::UserAgent->new;
$ua -> timeout(10);
$parser = new XML::Parser(Style => 'Tree');

use vars qw{ $info $events $stats @games %next @news $quit @local_news %channels @bcast $state $seis %topic %smslist $topiccmd };
use vars qw{ $last_update $last_irc $updater $irc_thread %req $last_save $lastsms };

share(@games);
share(@news);
share(@bcast);
share($quit);
share($state);
share(%smslist);
share($seis);
share($last_update);
share($last_irc);
share %next;
share($lastsms);

$info = &share({});
$events = &share({});
$stats = &share({});
%next = ();
@games = (300061449 .. 300061495, 300111111 .. 300111117);
@news = ();
@bcast = ();
@local_news = ();
$seis = 0;
%topic = ();
$topiccmd = 0;
%smslist = ();

$quit = 0;

$last_update = 0; 
$last_irc = 0;

$| = 1;


if (-r 'pallike.state') {
	require 'pallike.state';
}

# $info = &share($info);

my %feeds = (
	'stats' => \&fetch_stats,
	'allMatchEvents' => \&fetch_events,
);


$SIG{'INT'} = sub { $quit ++ };

$SIG{'TERM'} = $SIG{'INT'};

print "Starting main loop..\n";
while (1) {
	my $time = time() - 40;
	if ($last_update < $time) {
		finish_thread ($updater, 'updater');
		print "Starting update thread..\n";
		$updater = threads->create("update_thread");
	}

	if ($last_irc < $time) {
		finish_thread ($irc_thread, 'IRC thread');
		print "Starting IRC thread..\n";
		$irc_thread = threads->create("irc_thread");
	}

	last if $quit;
	
	sleep 10;
}

$updater -> join();
$irc_thread -> join();
print "Threads finished. Exiting..\n";

exit 0;

sub finish_thread {
	my $t = shift;
	my $name = shift;
	return unless defined($t);
	print "Finishing dead $name..\n";
	$t -> join();
}

sub irc_thread {
	my $irc = new Net::IRC;
	my $ic = '#pallike';
	my $conn = $irc -> newconn (
		Nick 	=> 'pallike',
		Server	=> 'irc.estpak.ee',
		Port	=> 6667,
		Ircname	=> 'jalgpallikoll',
		Pacing	=> 2.1,
	);

	$SIG{'INT'} = sub { $quit ++ };

	%channels = ($ic => 1);

	$conn -> add_handler('msg', \&on_msg);
	$conn -> add_handler('public', \&on_msg);
	$conn -> add_handler('topic', \&on_topic);

	$conn -> join($ic);
	$conn -> join("#futboliit"); $channels{'#futboliit'} = 1;
	
	$irc -> addconn($conn);
	while ($irc -> do_one_loop()) { };

	$irc -> timeout(0.01);
	
	print "Starting IRC loop..\n";
	while (1) {
		$last_irc = time();
		{
			lock(@bcast);
			if (scalar (@bcast) > 0) {
				refresh();
				
				my $msg = shift @bcast;
				foreach (grep { $channels{$_} } keys %channels) {
					$conn -> privmsg($_, utf8($msg) -> latin1);
				}
				select(undef, undef, undef, 0.1);
			}
		}

		{	
			lock($seis);
			if ($seis > 0) {
				refresh();
				
				my $topic_pref = join(", ", map { seis_topic($_); } get_games());
				print "topits: '$topic_pref', $seis\n";
				
				foreach (grep { $channels{$_} } keys %channels) {
					my @topic = $conn -> topic($_);
					print "Channel $_, topic: '$topic{$_}'\n";
					my $topic = $topic{$_};
					my $oldtopic = $topic;
					$topic =~ s/^([^\|]+)\|/$topic_pref \|/;
					if ($oldtopic ne $topic) {
						print "Trying to set new topic..\n";
						$conn -> topic($_, $topic);
					}
				}

	#			if ($topiccmd == 0) {
					sendsms($topic_pref, grep {$smslist{$_}} keys %smslist);
					$topiccmd = 0;
	#			}

				$seis --;
			} 
		}
	
		{
			lock(@news);	
			if (scalar (@news) + scalar(@local_news) > 0) {
				refresh();
					
				if (scalar(@news) > 0) {
					#lock @news;
					$_ = shift @news;
				} else {
					$_ = shift @local_news;
				}
				if (ref($_) ne 'ARRAY') {
					print "to $ic: $_\n";
					$conn -> privmsg($ic, utf8($_) -> latin1);
				} else {
					print "to: " . $_ -> [0] . " : '$_->[1]'\n";
					$conn -> privmsg($_->[0], utf8($_->[1]) -> latin1);
				}
				select(undef, undef, undef, 0.1);
			}
		}

#		if ($updater -> done()) {
#			print "Restarting update thread..\n";
#			$updater -> join();
#			$updater = threads->create("update_thread");
#		}
		
		$irc -> do_one_loop();
		threads->yield();

		if ($quit) {
			$conn -> quit("User requested quit");
			last;
		}
	}

	print "IRC loop exited..\n";
}

sub refresh {
	lock $state;
	if ($state) {
		eval($state);
	}
}

sub sendsms {
	my $txt = shift;

	if (scalar(@_) == 0) {
		print "polegi kellelegi meili saata.\n";
		return;
	}

	if ($lastsms eq $txt) {
		print "selline sms on juba saadetud?";
		return;
	}

	print "Sending email to: @_ : '$txt'\n";
	open (SMS, "|mail -s 'jalgpallisms' @_");
	print SMS "$txt";
	close (SMS);
	$lastsms = $txt;
}

sub matchday {
	my $date = shift;
	my $game = shift;

#	return 17 if ($game eq 1059192);
#	return 18 if ($game eq 1059193);
#	return 19 if ($game eq 1059194);

	return $date > 20100630 ? $date - 20100700 + 20 : $date - 20100610;
}

sub update_thread {
	$SIG{'INT'} = sub { $quit ++ };

	%req = map { 
		my $url = 'http://www.fifa.com/live/Competitions/worldcup/' .
			'matchcentrelight/MatchDay=' .
			matchday($info->{$_}->{date}, $_) .
			'/Day=1/Match=' . $_ . '/';
		print STDERR "$_: $url\n";
	
		$_ => $url;
	} @games;
	my @queue = keys %req;
	my %last_size = ();
	$last_save = 0;

	print "There is " . scalar(@queue) . " requests in queue\n";
	while (!$quit) {
		$last_update = time();
		@queue = 
			sort { $next{$a} <=> $next{$b} } 
				(grep { $next{$_} ne 'never' } (keys %req));

#		print "queue: ". join(", ", map { "$_: " . ($next{$_} - time) } @queue) . "\n";

		my $req = shift @queue;
		next if $next{$req} eq 'never';
		push @queue, $req;

		my $sleep = $next{$req} - time();
		$sleep = 5 if ($sleep > 5);
		$sleep = 1 if ($sleep < 0);
		sleep $sleep;
	
		next if (time < $next{$req});
		next unless defined($req{$req});
	
		{	
			if (time < $last_save + 300) {
				save_state();
				$last_save = time();
			}
		}

		my $uri;
		my $response = $ua -> request(HTTP::Request->new(GET =>
			$uri = $req{$req} . 'matchIndex.xml')); # ?ign=' . $info -> {$req} -> {'last_update'}));
		if ($response -> is_success) {
#			if (length($response -> content) != $last_size{$req}) {
#				$last_size{$req} = length($response -> content);
#				print "Got " . length($response -> content) . " bytes from $req\n";
#			}
			parse_xml($response->content);
			save_state(1);
			if (time < $last_save + 180) {
				save_state();
				$last_save = time();
			}
		} else {
			print STDERR "$uri: ", $response->status_line, "\n";
		}

		threads->yield();
		select(undef, undef, undef, 0.001);
	}

	print "Quitting. Saving state..\n";
	save_state();
}

sub get_games_real {
	my $arg = shift;
	my $fut = 0;

	my %groups = (
		'A' => 'RSA_URU_MEX_FRA',
		'B' => 'ARG_KOR_GRE_NGA',
		'C' => 'ENG_ALG_SVN_USA',
		'D' => 'GER_SRB_AUS_GHA',
		'E' => 'NED_JPN_DEN_CMR',
		'F' => 'ITA_NZL_SVK_PAR',
		'G' => 'CIV_POR_BRA_PRK',
		'H' => 'ESP_SUI_HON_CHI',
	);

	$arg = '' unless defined($arg);
	if ($arg =~ /fut./i) {
		$fut = 1;
		$arg =~ s/fut.//;
	}

	if ($groups{uc($arg)}) {
		$arg = $groups{uc($arg)};
	}

	my $old = setlocale(LC_TIME, "C");
	my $today = strftime("%d %B %Y", localtime(time));
	my $today_date = strftime("%Y%m%d", localtime(time));
	setlocale(LC_TIME, $old);
	
	if (uc($arg) eq 'FUT') {
		return grep { $info -> {$_} -> {'date'} >= $today_date } @games;
	}

	if (!defined($arg) || length($arg) < 3) {
		return grep { $info -> {$_} -> {'date'} eq $today_date } @games;
	} else {
		$arg = uc($arg);

		return grep {
			my $t1 = team0($_);
			my $t2 = team1($_);
			my $r = 0;

			if (length($arg) == 3) {
				$r = 1 if ($arg =~ /$t1/ || $arg =~ /$t2/);
			} else {
				$r = 1 if ($arg =~ /$t1/ && $arg =~ /$t2/);
			}

			$r = $r && ($info -> {$_} -> {phase} > -1 || $fut);

			$r;
		} @games;
	}
}

sub get_games {
	my @t = get_games_real(@_);
	print "get_games_real gave: @t\n";
	my @x = sort_by_time(@t);
	print "sorted: @x\n";
	return sort_by_time(@t);
}

sub sort_by_time {
	sort { $info -> {$a} -> {'date'} eq $info -> {$b} -> {'date'}
			 ? $info -> {$a} -> {'time'} cmp $info -> {$b} -> {'time'}
				: $info -> {$a} -> {'date'} cmp $info -> {$b} -> {'date'} } @_;
}

sub gamepref {
	my $g = shift;

	return team0($g) . ':' . team1($g);
}

sub seis {
	my $g = shift;
	return team0($g) . (
		!defined($info -> {$g} -> {'score'}) ? ':' :
			' ' . $info -> {$g} -> {'score'} . 
				($info -> {$g} -> {'penalties'} ? ' (' . $info -> {$g} -> {'penalties'} . ')' : '')
			. ' ') .
		team1($g) . 
		" -- " . $info -> {$g} -> {'time'}
}

sub seis_topic {
	my $g = shift;
	return !defined($info -> {$g} -> {'score'})  ? 
		team0($g) . ':' . team1($g) . '@' . substr($info -> {$g} -> {'time'}, $[, 5) :
		team0($g) . ' ' . $info -> {$g} -> {'score'} . 
			($info -> {$g} -> {'penalties'} ? ' (' . $info -> {$g} -> {'penalties'} . ')' : '') .
		' '.team1($g);
}

sub show_stats {
	my $g = shift;
	my $r = shift;

	foreach (sort keys %{$stats -> {$g}}) {
		reply ($r, latin1($stats -> {$g} -> {$_} -> [2]) -> utf8);
	}

	return;

	

	my $s = $stats -> {$g};
	my $t1 = team0($g);
	my $t2 = team1($g);

	reply ($r, "Palli valdamine:  $t1 $s->{'ballposession'} $t2");
	reply ($r, "Nurgalöögid:      $t1 $s->{'corners'} $t2");
	reply ($r, "Vead:             $t1 $s->{'fouls'} $t2");
	reply ($r, "Kauglöögid:       $t1 $s->{'shotswide'} $t2");
	reply ($r, "Tõrjed:           $t1 $s->{'blocked'} $t2");
	reply ($r, "Suluseisud:       $t1 $s->{'offside'} $t2");
	reply ($r, "Kollased kaardid: $t1 $s->{'yellowcard'} $t2");
	reply ($r, "Punased kaardid:  $t1 $s->{'redcard'} $t2");
	reply ($r, "Seis:             $t1 $s->{'goals'} $t2");
}

sub reply {
	my $r = shift;
	my $m = join('', @_);

	push @local_news, [ $r, $m ];
}

sub on_topic {
        my $self = shift;
        my $event = shift;

	my $channel = $event -> {args} -> [1];
	my $topic = $event -> {args} -> [2];

	print "received TOPIC: '$topic'\n";
	return if $topic eq '';

	$topic{$channel} = $topic;
}

sub on_msg {
        my $self = shift;
        my $event = shift;
        my $from = $event -> nick;

	$_ = join ("\n", $event -> args) . "\n";
	s/[\x00-\x1f]//g; # no bold crap.
	
	my $resp = ($event -> type eq 'public' ? $event -> to : $from);

	next unless /^!/;

	refresh();

	print "cmd: '$_'\n";
	my @cmd = split(' ', $_);
	my @args = @cmd[1 .. $#cmd];
	print "parsed cmd: '" . join("' '", @cmd) . "'\n";

	my %cmds;
	%cmds = ( 
		'!info' => sub {
			my $g;
			foreach $g (get_games(@args)) {
				my $gp = gamepref($g);
				reply ($resp, "$gp: " . $info -> {$g} -> {team0long}
					. " vs " . $info -> {$g} -> {team1long});
				reply ($resp, "$gp: Seis: " . $info -> {$g} -> {score});
				reply ($resp, "$gp: Penaltid: " . $info -> {$g} -> {penalties});
				reply ($resp, "$gp: Tuule kiirus: " . $info -> {$g} -> {wind} . " m/s");
				reply ($resp, "$gp: Temperatuur: " . $info -> {$g} -> {temperature} . " kraadi");
				reply ($resp, latin1("$gp: Õhuniiskus: " . $info -> {$g} -> {humidity} . "%") -> utf8);
				reply ($resp, "$gp: Ilm: " . $info -> {$g} -> {wconditions});
				reply ($resp, "$gp: Plats: " . $info -> {$g} -> {pconditions});

				#foreach (sort keys %{$info -> {$g}}) {
				#	reply($resp, "$gp: $_: " . $info -> {$g} -> {$_});
				#}
			}
		},
		'!stats' => sub {
			foreach (get_games(@args)) {
				show_stats($_, $resp);
			}
		},
		'!seis' => sub {
			foreach (get_games(@args)) {
				reply $resp, seis($_);
			}
		},
		'!today' => sub {
			my $old = setlocale(LC_TIME, "C");
			my $today = strftime("%d %B %Y", localtime(time));
			setlocale(LC_TIME, $old);
			print "täna on: '$today'\n";
			foreach (sort_by_time (grep { $info -> {$_} -> {'time'} =~ /$today/i } @games)) {
				reply $resp, seis($_);
			}
		},
		'!shots' => sub {
			my $g;
			foreach $g (get_games(@args)) {
				my $gp = gamepref($g);

				foreach (@{$info -> {$g} -> {shots}}) {
					reply  $resp, "$gp: " . $_ -> {'time'} . " " .
						$_ -> {'player'} . " (" . $_ -> {'team'} . ")";
				}
				
			}
		},
		'!help' => sub {
			reply $resp, "cmds: " . join(', ', (sort keys %cmds));
		},
		'!join' => sub {
			$self -> join($args[0]);
			$channels{$args[0]} = 1;
		},
		'!refresh' => sub {
			foreach (get_games(@args)) {
				print "$_ next was in " . ($next{$_} - time) . " seconds\n";
				$next{$_} = time() - 1;
			}
			reply $resp, "ok";
		},
		'!leave' => sub {
			$self -> part($args[0]);
			$channels{$args[0]} = 0;
		},
		'!group' => sub {
			my %pts = ();
			my %goals = ();
			my %diff = ();
			foreach (get_games(@args)) {
				print "$_: " . $info -> {$_} -> {score} . "\n";
				next unless $info -> {$_} -> {score} =~ /^\d+:\d+$/;
				my @d = split(':', $info -> {$_} -> {score});
				print "@d\n";
				$goals{team0($_)} += $d[0];
				$goals{team1($_)} += $d[1];

				$diff{team0($_)} += $d[0];
				$diff{team0($_)} -= $d[1];
				$diff{team1($_)} += $d[1];
				$diff{team1($_)} -= $d[0];

				
				if ($d[0] == $d[1]) {
					$pts{team0($_)} ++;
					$pts{team1($_)} ++;
				} elsif ($d[0] > $d[1]) {
					$pts{team0($_)} += 3;
					$pts{team1($_)} += 0;
				} else {
					$pts{team0($_)} += 0;
					$pts{team1($_)} += 3;
				}
			}

			print "pts: " . join (", ", map { "$_: $pts{$_}" } keys %pts) . "\n";
			print "goals: " . join (", ", map { "$_: $goals{$_}" } keys %goals) . "\n";
			print "diff: " . join (", ", map { "$_: $diff{$_}" } keys %diff) . "\n";

			reply ($resp,
				join(", ", map { 
					$_ . ": " . $pts{$_} . " (" . $diff{$_} . ", " . $goals{$_} . ")" 
				} (sort {
					($pts{$a} == $pts{$b}) ?
						$diff{$a} == $diff{$b} ?
							$goals{$b} <=> $goals{$a} 
						:	$diff{$b} <=> $diff{$a}
					:	$pts{$b} <=> $pts{$a};
				} (keys %pts))));
		},
		'!topic' => sub {
			lock($seis);
			print "Updating topic..\n";
			$seis++;
			$topiccmd ++;
		},
		'!listsms' => sub {
			reply ($resp, "kirja saavad: " . join(", ", sort (grep { $smslist{$_} } (keys %smslist))));
		},
		'!addsms' => sub {
			if ($args[0] =~ /^[\w\d\.\@\-]+$/) {
				$smslist{$args[0]} = 1;
				reply($resp, 'ok');
			} else {
				reply($resp, 'meil peab vastama maskile ^[\w\d\.@-]+$');
			}
		},
		'!removesms' => sub {
			$smslist{$args[0]} = 0;
			reply($resp, 'ok');
		},
		'!reboot' => sub {
			if ($args[0] eq 'now') {
				reply($resp, 'ok');
				$quit = 1;
			}
		}
	);

	if ($cmds{$cmd[0]}) {
		&{$cmds{$cmd[0]}}();
	} else {
		print "unknown command '$cmd[0]'.\n";
	}
}

sub save_state {
	my $internal_mode = shift;

	if ($internal_mode) {
		lock $state;
		$state = Data::Dumper -> Dump([$info, $events, $stats], 
			[qw{ $main::info $main::events $main::stats }]);
	} else {
		print "Saving state..\n";
		open(STATE, ">pallike.state") || die ("cannot save state?");
		print STATE "#!/usr/bin/perl\n\n";
		print STATE Data::Dumper -> Dump([$info, $events, $stats, \%next, \@games, \%smslist], 
			[qw{ $main::info $main::events $main::stats $tmp1 $tmp2 $tmp3 }]);
		
		print STATE "\%next = \%{\$tmp1};\n";
		print STATE "\@games = \@{\$tmp2};\n";
		print STATE "share(\%smslist);\n";
		print STATE "\%smslist = \%{\$tmp3};\n";
		foreach ('info', 'events', 'stats', 'next') {
	#		print STATE "share_r(\$main::$_);\n";
		}
		print STATE "\n1;\n";
		close(STATE);
	}
}

sub parse_xml {
	my $tree = $parser -> parse(shift);

#	print Dumper($tree) . "\n";

	my $m = parse_matchinfo(el($tree, 'matchmbm', 'matchinfo', 'match'));
	my $last_update = $tree -> [1] -> [0] -> {'timestamp'};
	$last_update =~ tr/T\-:.//d;
	$last_update = substr($last_update, $[, 14);
	$last_update--;
#	print "$m.last_update: $last_update\n";
	update ($m, 'last_update', $last_update);

	parse_events(el($tree, 'matchmbm', 'allevents'), $m);
	parse_feeds(el($tree, 'matchmbm', 'feeds'), $m);
#	parse_statistics($ -> [1] -> [6], $m);
}

sub parse_stats {
	my $m = shift;
	my $tree = $parser -> parse(shift);

	parse_statistics(el($tree, 'matchmbm', 'statistics'), $m);
}

sub parse_allEvents {
	my $m = shift;
	my $tree = $parser -> parse(shift);

	parse_events(el($tree, 'matchmbm', 'allevents'), $m);
}

sub el {
	my $branch = shift;
	my $key = shift;

	my $found = 0;

	for (@{$branch}) {
		next if (ref($_) eq 'HASH');
		return scalar(@_) > 0 ? el($_, @_) : $_ if $found > 0;
		if ($found < 0) {
			$found = 0;
			next;
		}

		$found = $_ eq $key ? 1 : -1;
	}

	return undef;
}

sub update {
	my $match = shift;
	my $key = shift;
	my $val = shift;
	my $news = shift;
	my $bcast = shift;
	my $code = shift;

#	print "update: $match, $key, $val\n";

	if (ref($info -> {$match}) ne 'HASH') {
		my %h :shared;
		$info -> {$match} = \%h;
	}

	$val = trim($val);

	if (defined($val) 
		&& (!defined($info -> {$match} -> {$key}) 
			|| $info -> {$match} -> {$key} ne $val)
		&& ($news || $bcast)) {
		if ($news) {
			lock @news;
			push @news, $news;
		}

		if ($bcast) {
			lock @bcast;
			push @bcast, $bcast;
		}

		if ($code) {
			&$code();
		}
	}

	$info -> {$match} -> {$key} = $val;
}

sub team0 {
	my $m = shift;
	$info -> {$m} -> {team0};
}

sub team1 {
	my $m = shift;
	$info -> {$m} -> {team1};
}

sub fixtime {
	$_ = shift;

	s/^\S+ (\d+ .... 2010) - (..:..)$/$2, $1/;

	s/^22:/23:/;
	s/^21:/22:/;
	s/^20:/21:/;
	s/^19:/20:/;
	s/^18:/19:/;
	s/^17:/18:/;
	s/^16:/17:/;
	s/^15:/16:/;
	s/^14:/15:/;
	s/^13:/14:/;

	$_;
}

sub fixtime2 {
	my $t = shift;

#	print $t -> {'year'} . ", " . $t -> {'month'} . ", " . $t -> {'day'} . ".\n";
	my $out = $t -> {'year'} . (uc($t -> {'month'}) eq uc('june') ? '06' :'07') .
		sprintf("%02d", $t -> {'day'});
#	print "fixtime2: '$out'\n";
	return $out;
}

sub gameover {
	my $m = shift;
	my $today = strftime("%Y%m%d", localtime(time));	
	return $info -> {$m} -> {'date'} < $today;
}

sub update_seis {
	print "seis muutus.";
	save_state(1);
	lock $seis;
        $seis ++;	
}

sub parse_matchinfo {
	my $mi = shift;

	my $m = $mi -> [0] -> {code};

	my $x = 0;
	my %match = map { s/^team$/team_$x/ && $x++; $_ } @{$mi}[1 .. $#{$mi}];

	my %weather = %{$match{'wconditions'} -> [0]};

	update($m, 'temperature', $weather{'temperature'}, 
		gamepref($m) . " Temperatuur: $weather{'temperature'} kraadi");
		
	update($m, 'wind', $weather{'wind'}, 
		gamepref($m) . " Tuule kiirus: $weather{'wind'} m/s");
		
	update($m, 'humidity', $weather{'humidity'}, 
		gamepref($m) . " Õhuniiskus: $weather{'humidity'}%");
		
	update($m, 'wconditions', $match{'wconditions'} -> [2], 
		gamepref($m) . " Ilm: $match{'wconditions'}->[2]");
		
	update($m, 'pconditions', $match{'pconditions'} -> [2], 
		gamepref($m) . " Plats: $match{'pconditions'}->[2]");
		
	update($m, 'team0', uc($match{'team_0'} -> [0] -> {countrycode}), undef);
	update($m, 'team0long', $match{'team_0'} -> [0] -> {name}, undef);
	update($m, 'team0short', $match{'team_0'} -> [0] -> {shortname}, undef);
	update($m, 'team1', uc($match{'team_1'} -> [0] -> {countrycode}), undef);
	update($m, 'team1long', $match{'team_1'} -> [0] -> {name}, undef);
	update($m, 'team1short', $match{'team_1'} -> [0] -> {shortname}, undef);
	update($m, 'time', fixtime($mi -> [0] -> {'date'}), undef);
	update($m, 'date', fixtime2($mi -> [0]), undef);

	my $phase = $match{'maxphase'} -> [0] -> {'live'};
	my $score = trim($match{'score'} -> [2]);
	$score = undef if $phase < 0;

	update($m, 'score', $score,
		" Seis: " . team0($m) ." $score " . team1($m),
		" Seis: " . team0($m) ." $score " . team1($m), \&update_seis);

	my $penalties = trim($match{'penalties'} -> [2]);
	update($m, 'penalties', $penalties,
			" Penaltid: " . team0($m) ." $penalties " . team1($m),
			" Penaltid: " . team0($m) ." $penalties " . team1($m), \&update_seis);

	update($m, 'phase', $match{'maxphase'} -> [0] -> {'live'}, undef);
#		"Faas: " . $match{'maxphase'} -> [0] -> {'live'});

	update($m, 'infoarea', $match{'infoArea'} -> [2], undef);
#	if ($phase > 4 && $info -> {$m} -> {'infoarea'} =~ ) {
#	}

	if (gameover($m)) {
		$next{$m} = 'never';
	} elsif ($phase > -1) {
		$next{$m} = time() + 10 + int(rand(10))
	} else {
		$next{$m} = time() + 5*60 + int(rand(3*60));
	}

#	print STDERR "$m: $phase: seconds to next check: " . ($next{$m} - time()) . "\n";

	save_state(1);

	return $m;
}

sub trim {
	my $x = shift;
	return undef unless defined($x);
	$x =~ s/^\s+//;
	$x =~ s/\s+$//;
	$x =~ s/\n//;
	$x = undef if $x =~ /^\s*$/;
	$x;
}

sub parse_feeds {
	my $feeds = shift;
	my $m = shift;

	my $feed;
	for $feed (@{$feeds}) {
		next unless ref($feed) eq 'ARRAY';
		my $type = $feed -> [0] -> {'type'};
		next unless exists $feeds{$type};
		my $timestamp = $feed -> [0] -> {'timestamp'};
		next unless defined($timestamp) && $timestamp > 0;
		next if defined($info -> {$m} -> {$type . '_timestamp'}) 
			&& $info -> {$m} -> {$type . '_timestamp'} eq $timestamp;
		if (&{$feeds{$type}}($m, $info -> {$m} -> {$type . '_timestamp'})) {
			update($m, $type . '_timestamp', $timestamp);
		}
	}
}

sub fetch_stats {
	my $m = shift;
	my $old_timestamp = shift;
	print "Fetching stats for $m\n";

	my $uri = '';

	$old_timestamp = '' unless defined($old_timestamp);

	my $response = $ua -> request(HTTP::Request->new(GET =>
		$uri = $req{$m} . 'stats.xml?ign=' . $old_timestamp));

	print "stats request '$uri'\n";
	if ($response -> is_success) {
		parse_stats($m, $response->content);
		save_state(1);
		if (time < $last_save + 120) {
			save_state();
			$last_save = time();
		}
	} else {
		print STDERR "$uri: ", $response->status_line, "\n";
		return 0;
	}

	1;
}

sub fetch_events {
	my $m = shift;
	my $old_timestamp = shift;
	print "Fetching events for $m\n";

	my $uri = '';

	$old_timestamp = '' unless defined($old_timestamp);

	my $response = $ua -> request(HTTP::Request->new(GET =>
		$uri = $req{$m} . 'allMatchEvents.xml')); # ?ign=' . $old_timestamp));

	print "stats request '$uri'\n";
	if ($response -> is_success) {
		parse_allEvents($m, $response->content);
		save_state(1);
		if (time < $last_save + 120) {
			save_state();
			$last_save = time();
		}
	} else {
		print STDERR "$uri: ", $response->status_line, "\n";
		return 0;
	}

	1;
}

sub update_stats {
	my $m = shift;
	my $t1 = shift;
	my $t2 = shift;
	my $key = shift;
	my $legend = shift;

	$t1 -> {$key} = trim($t1 -> {$key});
	$t2 -> {$key} = trim($t2 -> {$key});

	return unless defined($t1 -> {$key}) && defined($t2 -> {$key});

	if (ref($stats -> {$m}) ne 'HASH') {
		my %h : shared;
		$stats -> {$m} = \%h;
	}

	my $new = 0;
	if (ref($stats -> {$m} -> {$key}) ne 'ARRAY') {
		my @l : shared;
		$stats -> {$m} -> {$key} = \@l;
		$new = 1;
	}

	if ($new || ($stats -> {$m} -> {$key} -> [0] ne $t1 -> {$key}) ||
		($stats -> {$m} -> {$key} -> [1] ne $t2 -> {$key})) {
		$stats -> {$m} -> {$key} -> [0] = $t1 -> {$key};
		$stats -> {$m} -> {$key} -> [1] = $t2 -> {$key};
		$stats -> {$m} -> {$key} -> [2] =
			$legend . team0($m) . sprintf(" %3d:%-3d ", 
				$t1 -> {$key}, $t2 -> {$key}) .
				team1($m);

		lock @news;
		push @news, $stats -> {$m} -> {$key} -> [2];
		print "statsiparser: " . $stats -> {$m} -> {$key} -> [2] . "\n";
	}
}

sub parse_statistics {
	my $s = shift;
	my $m = shift;

	my @t = grep { ref($_) eq 'ARRAY' } @{$s};

	my ($t1, $t2);

	$t1 = { map { ref($_) eq 'ARRAY' ? $_ -> [2] : $_ } @{$t[0]}[1 .. $#{$t[0]}] };
	$t2 = { map { ref($_) eq 'ARRAY' ? $_ -> [2] : $_ } @{$t[1]}[1 .. $#{$t[1]}] };

	update_stats($m, $t1, $t2, 'ballpossession',	'Palli valdamine: ');
	update_stats($m, $t1, $t2, 'corners',		'Nurgalööke:      ');
	update_stats($m, $t1, $t2, 'fouls',		'Vigu:            ');
	update_stats($m, $t1, $t2, 'shotswide',		'Kauglööke:       ');
	update_stats($m, $t1, $t2, 'shotsongoal',	'Pealelööke:      ');
	update_stats($m, $t1, $t2, 'blocked',		'Tõrjeid:         ');
	update_stats($m, $t1, $t2, 'totalshots',	'Pealelööke kokku:');
	update_stats($m, $t1, $t2, 'yellowcard',	'Kollaseid kaarte:');
	update_stats($m, $t1, $t2, 'redcard',		'Punaseid kaarte: ');
	update_stats($m, $t1, $t2, 'offside',		'Suluseise:       ');
	update_stats($m, $t1, $t2, 'goals',		'Väravaid:        ');
}

sub parse_events {
	my $me = shift;
	my $m = shift;

	if (ref($events -> {$m}) ne 'HASH') {
		my %h :shared;
		$events -> {$m} = \%h;
	}

	while (scalar(@{$me}) > 1) {
		parse_event(pop @{$me}, $m);
		pop @{$me};
	}
}

# event types
# 1 - player "is shown a yellow card"
# 2 -
# 3 - player "scores (from the penalty shot)"
# 4 - vahetus 
# 5 -
# 6 - player (team) "has a shot b#locked"
# 7 -
# 
# 11 - player "commits a foul"

sub parse_event {
	my $e = shift;
	my $m = shift;

	return unless ref($e) eq 'ARRAY';

	my %h : shared = ('id' => $e -> [0] -> {id} );

	my $o = \%h;

	return if (ref($events -> {$m} -> {$o -> {id}}) eq 'HASH');

	my %m = @{$e}[1 .. $#{$e}];
	
	return if scalar(keys %{$e -> [0]}) == 1; # <e id=blaah/>


	$o -> {'code'} = $e -> [0] -> {code};
	$o -> {'time'} = $e -> [0] -> {'min'} . ':' .
		sprintf("%02d", $e -> [0] -> {'sec'}) . 
		($e -> [0] -> {timedisplay} ? ' (' . $e -> [0] -> {timedisplay} . ')' : '');
	$o -> {'desc'} = $m{'descr'} -> [2] if defined($m{'descr'});
#	$o -> {'comm'} = $m{'comment'} -> [2];

	print "ep: " . $o -> {'code'} . ' @ ' . $o -> {'time'} . ', d: "' . $o -> {'desc'} . '", c: "' .
		$o -> {'comm'} . '"' . "\n";

	if ($o -> {'desc'}) {
		lock @news;
		push @news, team0($m) . '-' . team1($m) . ': ' .$o -> {'time'} . ': ' . $o -> {'desc'};
		print "eventparser: " . team0($m) . '-' . team1($m) . ': ' .$o -> {'time'} . ': ' . $o -> {'desc'} . "\n";
	}

	$events -> {$m} -> {$o -> {id}} = $o;

#	print Dumper($e);
}

sub share_r {
	my $ref = shift;

	print "sharing '$ref'\n";

	if (ref($ref) eq 'HASH') {
		print "Sharing hash with " . scalar(keys %{$ref}) . " keys\n";
		print Dumper($ref);
		foreach (keys %{$ref}) {
			share_r($ref -> {$_});
		}
		share(%{$ref});
	} elsif (ref($ref) eq 'ARRAY') {
		print "Sharing array with " . scalar(@{$ref}) . " elemnts\n";
		foreach (@{$ref}) {
			share_r($_);
		}
		share(@{$ref});
	} elsif (ref($ref) eq 'SCALAR') {
		print "Sharing scalar.\n";
		share_r(${$ref});
	}

	share $ref;
}

