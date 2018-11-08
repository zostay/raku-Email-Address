unit class Email::Address;
use v6;

# no precompilation;
# use Grammar::Tracer;

grammar RFC5234-Parser {
    # ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z
    token alpha      { <[ \x[41]..\x[5a] \x[61]..\x[7a] ]> }

    # DIGIT          =  %x30-39
    #                        ; 0-9
    token digit      { <[ \x[30]..\x[39] ]> }

	# CR             =  %x0D\r
	#                        ; carriage return\r
	token cr         { \x[0d] }

	# LF             =  %x0A\r
	#                        ; linefeed\r
	token lf         { \x[0a] }

	# CRLF           =  CR LF\r
	#                        ; Internet standard newline\r
	token crlf       { <cr> <lf> }

	# DQUOTE         =  %x22\r
	# 					; " (Double Quote)\r
	token dquote     { \x[22] }

	# HTAB           =  %x09\r
	#                   ; horizontal tab\r
	token htab       { \x[09] }

	# SP             =  %x20\r
	token sp         { \x[20] }

    # WSP            =  SP / HTAB\r
    #                        ; white space\r
	token wsp        { <sp> || <htab> }

    # VCHAR          =  %x21-7E
    #                        ; visible (printing) characters
    token vchar      { <[ \x[21]..\x[7e] ]> }
}

grammar RFC5322-Parser is RFC5234-Parser {
    token TOP         { <address-list> }

    # address         =   mailbox / group\r
	token address     { <mailbox> | <group> }

    # mailbox         =   name-addr / addr-spec\r
	token mailbox     { <name-addr> | <addr-spec> }

    # name-addr       =   [display-name] angle-addr\r
    token name-addr   { <display-name>? <angle-addr> }

    # angle-addr      =   [CFWS] "<" addr-spec ">" [CFWS] /\r
    #                     obs-angle-addr\r
    token angle-addr  { <.cfws>? '<' <addr-spec> '>' <.cfws>? |
                        <obs-angle-addr> }

    # group           =   display-name ":" [group-list] ";" [CFWS]\r
    token group       { <display-name> ':' <group-list>? ';' }

    # display-name    =   phrase\r
    token display-name { <phrase> }

    # mailbox-list    =   (mailbox *("," mailbox)) / obs-mbox-list\r
    token mailbox-list { [ <mailbox>+ % ',' ] | <obs-mbox-list> }

    # address-list    =   (address *("," address)) / obs-addr-list\r
    token address-list { [ <address>+ % ',' ] | <obs-addr-list> }

    # group-list      =   mailbox-list / CFWS / obs-group-list\r
    token group-list  { <mailbox-list> | <cfws> | <obs-group-list> }

    # addr-spec       =   local-part "@" domain\r
    token addr-spec   { <local-part> '@' <domain> }

    # local-part      =   dot-atom / quoted-string / obs-local-part\r
    token local-part  { <dot-atom> | <quoted-string> | <obs-local-part> }

    # domain          =   dot-atom / domain-literal / obs-domain\r
    token domain      { <dot-atom> | <domain-literal> | <obs-domain> }

    # domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]\r
    token domain-literal { $<pre-literal> = [ <cfws>? ] '[' $<literal> = [ [ <fws>? <dtext> ]* <fws> ] ']' }

    # dtext           =   %d33-90 /          ; Printable US-ASCII\r
    #                     %d94-126 /         ;  characters not including\r
    #                     obs-dtext          ;  "[", "]", or "\\"\r
    token dtext       { <[ \x[21]..\x[5a] ]> |
                        <[ \x[5e]..\x[7e] ]> |
                        <obs-dtext> }

    # word            =   atom / quoted-string\r
    token word        { <atom> | <quoted-string> }

    # phrase          =   1*word / obs-phrase\r
    token phrase      { <word>+ | <obs-phrase> }

    # atext           =   ALPHA / DIGIT /    ; Printable US-ASCII\r
    #                     "!" / "#" /        ;  characters not including\r
    #                     "\$" / "%" /        ;  specials.  Used for atoms.\r
    #                     "&" / "'" /\r
    #                     "*" / "+" /\r
    #                     "-" / "/" /\r
    #                     "=" / "?" /\r
    #                     "^" / "_" /\r
    #                     "`" / "{" /\r
    #                     "|" / "}" /\r
    #                     "~"\r
    token atext       { <alpha> | <digit> |
                        '!' | '#' |
                        '\$' | '%' |
                        '&' | "'" |
                        '*' | '+' |
                        '-' | '/' |
                        '=' | '?' |
                        '^' | '_' |
                        '`' | '{' |
                        '|' | '}' |
                        '~' }

    # atom            =   [CFWS] 1*atext [CFWS]\r
    token atom        { $<pre> = [ <cfws>? ] <atext>+ $<post> = [ <cfws>? ] }

    # dot-atom-text   =   1*atext *("." 1*atext)\r
    token dot-atom-text { $<atexts> = [ <atext>+ ]+ % '.' }

    # dot-atom        =   [CFWS] dot-atom-text [CFWS]\r
    token dot-atom    { $<pre> = [ <cfws>? ] <dot-atom-text> $<post> = [ <cfws>? ] }

    # FWS             =   ([*WSP CRLF] 1*WSP) /  obs-FWS\r
    #                                        ; Folding white space\r
    token fws         { [ [ <wsp>* <crlf> ]? <wsp>+ ] | <obs-fws> }

    # ctext           =   %d33-39 /          ; Printable US-ASCII\r
    #                     %d42-91 /          ;  characters not including\r
    #                     %d93-126 /         ;  "(", ")", or "\"\r
    #                     obs-ctext\r
    token ctext       { <[ \x[21]..\x[27]
                           \x[2a]..\x[5b]
                           \x[5d]..\x[7e] ]> |
                        <obs-ctext> }

    # ccontent        =   ctext / quoted-pair / comment\r
    token ccontent    { <ctext> | <quoted-pair> | <comment> }

    # comment         =   "(" *([FWS] ccontent) [FWS] ")"\r
    token comment     { '(' $<comment-content> = [ [ <fws>? <ccontent> ]* <fws>? ] ')' }

    # CFWS            =   (1*([FWS] comment) [FWS]) / FWS\r
    token cfws        { [ [ $<pres> = [ <fws>? ] <comment> ]+ $<post> = [ <fws>? ] ] | $<orelse> = <fws> }

    # obs-FWS         =   1*WSP *(CRLF 1*WSP)\r
    token obs-fws     { <wsp>+ [ <crlf> <wsp>+ ]* }

    # qtext           =   %d33 /             ; Printable US-ASCII\r
    #                     %d35-91 /          ;  characters not including\r
    #                     %d93-126 /         ;  "\" or the quote character\r
    #                     obs-qtext\r
    token qtext       { <[ \x[21]
                           \x[23]..\x[5b]
                           \x[5d]..\x[7e] ]> |
                        <obs-qtext> }

    # qcontent        =   qtext / quoted-pair\r
    token qcontent    { <qtext> | <quoted-pair> }

    # quoted-string   =   [CFWS]\r
    #                     DQUOTE *([FWS] qcontent) [FWS] DQUOTE\r
    #                     [CFWS]\r
    token quoted-string { <.cfws>?
                          <.dquote> $<quoted-string> = [ [ <.fws>? <qcontent> ]* <.fws>? ] <.dquote>
                          <.cfws>? }

    # obs-NO-WS-CTL   =   %d1-8 /            ; US-ASCII control\r
    #                     %d11 /             ;  characters that do not\r
    #                     %d12 /             ;  include the carriage\r
    #                     %d14-31 /          ;  return, line feed, and\r
    #                     %d127              ;  white space characters\r
    token obs-no-ws-ctl { <[ \x1..\x8
                             \xb
                             \xc
                             \xe..\x[1f]
                             \x[7f] ]> }

    # obs-ctext       =   obs-NO-WS-CTL\r
    token obs-ctext   { <obs-no-ws-ctl> }

    # obs-qtext       =   obs-NO-WS-CTL\r
    token obs-qtext   { <obs-no-ws-ctl> }

    # obs-qp          =   "\\" (%d0 / obs-NO-WS-CTL / LF / CR)\r
    token obs-qp      { '\\' [ \0 | <obs-no-ws-ctl> | <lf> | <cr> ] }

    # obs-phrase      =   word *(word / "." / CFWS)\r
    token obs-phrase  { $<head> = <word> $<tail> = [ <word> | '.' | <cfws> ]* }

    # quoted-pair     =   ("\\" (VCHAR / WSP)) / obs-qp\r
    token quoted-pair { [ '\\' [ <vchar> | <wsp> ] ] | <obs-qp> }

    # obs-angle-addr  =   [CFWS] "<" obs-route addr-spec ">" [CFWS]
    token obs-angle-addr { <cfws>? '<' <obs-route> <addr-spec> '>' <cfws>? }

    # obs-route       =   obs-domain-list ":"
    token obs-route   { <obs-domain-list> ':' }

    # obs-domain-list =   *(CFWS / ",") "@" domain
    #                     *("," [CFWS] ["@" domain])
    token obs-domain-list { [ <.cfws> | ',' ]* '@' $<head> = <domain>
                            $<tail> = [ ',' <.cfws>? [ '@' <domain> ]? ]* }

    # obs-mbox-list   =   *([CFWS] ",") mailbox *("," [mailbox / CFWS])
    token obs-mbox-list { [ <cfws>? ',' ]* $<head> = <mailbox> $<tail> = [ ',' [ <mailbox> | <cfws> ]? ]* }

    # obs-addr-list   =   *([CFWS] ",") address *("," [address / CFWS])
    token obs-addr-list { [ <cfws>? ',' ]* <address> [ ',' [ <address> | <cfws> ]? ]* }

    # obs-group-list  =   1*([CFWS] ",") [CFWS]
    token obs-group-list { [ <cfws>? ',' ]+ <cfws>? }

    # obs-local-part  =   word *("." word)
    token obs-local-part { <word>+ % '.' }

    # obs-domain      =   atom *("." atom)
    token obs-domain  { <atom>+ % '.' }

    # obs-dtext       =   obs-NO-WS-CTL / quoted-pair
    token obs-dtext   { <obs-no-ws-ctl> | <quoted-pair> }
}

class RFC5322-Actions {
    sub unfold-fws($_) { S:global/ "\r\n" ( " " | "\t" ) /$0/ }
    sub unquote-pairs($_) { S:global/ "\\" ( " " | "\t" | "\0" | <[ \x1..\x8 \xb \xc \xe..\x[1f] \x[7f] ]> | "\n" | "\r" ) /$0/ }

    method TOP($/) { make $<address-list>.made }
    method address($/) { make $<mailbox>.made // $<group>.made }
    method mailbox($/) { make $<name-addr>.made // $<addr-spec>.made }
    method name-addr($/) {
        make %(
            type         => 'mailbox',
            display-name => $<display-name>.made,
            address      => $<angle-addr>.made,
            comment      => $*comments.drain,
            original     => ~$/,
        )
    }
    method angle-addr($/) { make $<addr-spec>.made // $<obs-angle-addr>.made }
    method group($/) {
        make %(
            type         => 'group',
            display-name => $<display-name>.made,
            mailbox-list => $<group-list>.made,
            original     => ~$/,
        )
    }
    method display-name($/) { make $<phrase>.made }
    method mailbox-list($/) { make $<mailbox>».made // $<obs-mbox-list>.made }
    method address-list($/) { make $<address>».made // $<obs-addr-list>.made }
    method group-list($/) { make $<mailbox-list>.made // $<obs-group-list>.made // [] }
    method addr-spec($/) {
        make %(
            local-part => $<local-part>.made,
            domain     => $<domain>.made,
            original   => ~$/,
        )
    }
    method local-part($/) { make $<dot-atom>.made // $<quoted-string>.made // $<obs-local-part>.made }
    method domain($/) { make $<dot-atom>.made // $<domain-literal>.made // $<obs-domain>.made }
    method domain-literal($/) {
        make ($<pre-literal>.made ~ "[$<literal>]").&unfold-fws.&unquote-pairs
    }
    method word($/) { make $<atom>.made // $<quoted-string>.made }
    method phrase($/) { make [~] @($<word>».made) // $<obs-phrase>.made }
    method atom($/) { quietly make $<pre>.made ~ ([~] $<atext>) ~ $<post>.made }
    method dot-atom-text($/) { make [~] $<atexts> }
    method dot-atom($/) { quietly make $<pre>.made ~ $<dot-atom-text>.made ~ $<post>.made }
    method quoted-string($/) { make "$<quoted-string>".&unquote-pairs }
    method comment($/) { $*comments.append("$<comment-content>".&unquote-pairs) }
    method cfws($/) { quietly make [~] |$<pres>, $<post> }
    method obs-phrase($/) {
        make $<head>.made ~ $<tail>.map({
            when '.' { '.' }
            default { .made }
        });
    }
    method obs-angle-addr($/) {
        my %address = $<addr-spec>.made;
        %address<local-part> = $<obs-route>.made ~ %address<local-poart>;
        make %address;
    }
    method obs-route($/) { make $<obs-domain-list>.made ~ ':' }
    method obs-domain-list($/) {
        make join ',', ($<head>.made, |$<tail>.map({ $<domain>.made }));
    }
    method obs-mbox-list($/) {
        make ($<head>.made, |$<tail>».made);
    }
}

my sub _parse($str, :$parser, :$actions, :$rule) {
    my $*comments = class {
        has $.comment;
        method drain() { my $c = $!comment; $!comment = Nil; $c }
        method append($c) {
            with $!comment { $!comment ~= $c }
            else { $!comment = $c }
        }
    }.new;

    $parser.parse($str, :$actions, :$rule).made;
}

my sub is-atext($c) { ?RFC5322-Parser.parse($c, :rule<atext>) }

my sub char-needs-escape($c) {
    $c eq '"' | Q'\' | '\0' | '\t' | '\n' | '\r'
}

my proto maybe-escape(|) { * }
multi maybe-escape("", :$quote-dot) { '""' }
multi maybe-escape($data, :$quote-dot is copy = False) {
    # leading or trailing dot is always quoted
    $quote-dot++ if $data.starts-with('.') || $data.ends-with('.');

    # is quoting needed otherwise?
    my $chars = $data.comb;
    if so $chars.first({ !is-atext($_) && ($quote-dot || $_ ne '.') }) {
        # quote and escape
        if so $chars.first({ char-needs-escape($_) }) {
            qq["{[~] $chars.map({
                char-needs-escape($_) ?? "\\$_" !! $_
            })}"];
        }

        # only quote
        else {
            qq["$data"];
        }
    }

    # no quote or escape
    else {
        $data;
    }
}

my sub has-mime-word($str) { $str.contains("=?") }

role AddrSpec {
    has Str $.local-part is rw;
    has Str $.domain is rw;

    method format(--> Str:D) {
        join '@', maybe-escape($!local-part), $!domain
    }

    method Str(--> Str:D) { self.format }
}

my class AddrSpec::Parsed does AddrSpec {
    has Str $.original;
}

subset CommentStr of Str where {
    # Only match strings that contain balanced parentheses
    my $balanced-parens;
    $balanced-parens = regex {
        [
        | <-[()]>
        | '()'
        | '(' ~ ')' $balanced-parens
        ]+
    }

    !.defined || /^ $balanced-parens $/
};

role Mailbox {
    has Str $.display-name is rw;
    has AddrSpec $.address is rw;
    has CommentStr $.comment is rw;

    method local-part(--> Str) is rw { return-rw $!address.local-part }
    method domain(--> Str) is rw { return-rw $!address.domain }

    method set-address(
        Str:D $address,
        :$parser = RFC5322-Parser,
        :$actions = RFC5322-Actions,
    ) {
        my %addr-spec = _parse($address, :$parser, :$actions, :rule<addr-spec>);
        $!address.local-part = %addr-spec<local-part>;
        $!address.domain     = %addr-spec<domain>;
        $!address;
    }

    method guess-name(--> Str) {
        with $!display-name { return $!display-name if $!display-name.chars }
        with $!comment { return $!comment if $!comment.chars }
        with $.local-part { return $.local-part }
        '';
    }

    method format(--> Str) {
        my $address = '';

        # quoting can't be used when =?...?...?= mime words are in the name,
        # use obsolete RFC822 display name instead in that case. Since we don't
        # make any effort to understand or decode these, we assume we'll
        # just encounter them as-is but do this one special thing for them
        with $!display-name {
            if has-mime-word($!display-name) {
                $address ~= $!display-name;
            }
            else {
                $address ~= maybe-escape($!display-name);
            }

            $address ~= " <$!address>";
        }

        else {
            $address ~= $!address;
        }

        $address ~= " ($!comment)" with $!comment;

        $address;
    }

    method Str(--> Str) { self.format }
}

my class Mailbox::Parsed does Mailbox {
    has Str $.original;
}

role Group {
    has Str $.display-name is rw;
    has Mailbox @.mailbox-list;

    method !parse-if-needed(@addresses, :$parser, :$actions) {
        gather for @addresses {
            when Mailbox { .take }
            default {
                my $mailbox = _parse(~$_, :$parser, :$actions, :rule<mailbox>);
                take Mailbox::Parsed.new(
                    display-name => $mailbox<display-name>,
                    address      => $mailbox<address>,
                    comment      => $mailbox<comment>,
                    original     => $_,
                );
            }
        }
    }

    multi method new(::?CLASS:U:
        Str $display-name,
        *@addresses,
        :$parser = RFC5322-Parser,
        :$actions = RFC5322-Actions,
    ) {
        self.bless:
            :$display-name,
            mailbox-list => self!parse-if-needed(@addresses, :$parser, :$actions),
            ;
    }

    method format(--> Str) {
        my $group = '';

        # quoting can't be used when =?...?...?= mime words are in the name,
        # use obsolete RFC822 display name instead in that case. Since we don't
        # make any effort to understand or decode these, we assume we'll
        # just encounter them as-is but do this one special thing for them
        if has-mime-word($!display-name) {
            $group ~= $!display-name;
        }
        else {
            $group ~= maybe-escape($!display-name);
        }

        $group ~= ': ';
        $group ~= join(', ', .format) for @!mailbox-list;
        $group ~= ';';
    }

    method Str(--> Str) { self.format }
}

my class Group::Parsed does Group {
    has Str $.original;
}

my sub build-addr-spec(
    %spec,
    :$addr-spec-class = AddrSpec::Parsed,
) {
    $addr-spec-class.new(
        local-part => .<local-part>,
        domain     => .<domain>,
        original   => .<original>,
    ) with %spec;
}

my sub build-mailbox(
    %spec,
    :$mailbox-class = Mailbox::Parsed,
    :$addr-spec-class = AddrSpec::Parsed,
) {
    $mailbox-class.new(
        display-name => .<display-name>,
        address      => build-addr-spec(.<address>, :$addr-spec-class),
        comment      => .<comment> // Str,
        original     => .<original>,
    ) given %spec;
}

my sub build-group(
    %spec,
    :$group-class = Group::Parsed,
    :$mailbox-class = Mailbox::Parsed,
    :$addr-spec-class = AddrSpec::Parsed,
) {
    $group-class.new(
        display-name => .<display-name>,
        mailbox-list => .<mailbox-list>.map({ build-mailbox($_, :$mailbox-class, :$addr-spec-class) }),
        original     => .<original>,
    ) with %spec;
}

multi method parse(::?CLASS:U: Str $str, :$mailboxes!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Seq) {
    gather for _parse($str, :$parser, :$actions, :rule<mailbox-list>) {
        take build-mailbox($_);
    }
}

multi method parse(::?CLASS:U: Str $str, :$groups!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Seq) {
    gather for _parse($str, :$parser, :$actions, :rule<group-list>) {
        take build-group($_);
    }
}

multi method parse(::?CLASS:U: Str $str, :$addresses!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Seq) {
    gather for _parse($str, :$parser, :$actions, :rule<address-list>) {
        when so .<type> eq 'mailbox' { take build-mailbox($_) }
        when so .<type> eq 'group' { take build-group($_) }
    }
}

multi method parse-one(::?CLASS:U: Str $str, :$mailbox!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Mailbox) {
    build-mailbox($_)
        given _parse($str, :$parser, :$actions, :rule<mailbox>);
}

multi method parse-one(::?CLASS:U: Str $str, :$group!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Group) {
    build-group($_)
        given _parse($str, :$parser, :$actions, :rule<group>);
}

multi method parse-one(::?CLASS:U: Str $str, :$address!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Any) {
    given _parse($str, :$parser, :$actions, :rule<address>) {
        when so .<type> eq 'mailbox' { take build-mailbox($_) }
        when so .<type> eq 'group' { take build-group($_) }
    }
}


multi method format(::?CLASS:U: *@addresses --> Str) {
    join ', ', @addresses».format;
}

multi method compose(::?CLASS:U: Str $local-part, Str $domain --> Str) {
    AddrSpec.new(:$local-part, :$domain).format
}

multi method split(::?CLASS:U: Str $address, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> List) {
    (.<local-part>, .<domain>)
        given _parse($address, :$parser, :$actions, :rule<addr-spec>);
}

=begin pod

=head1 NAME

Email::Address - parse and format RFC 5322 email addresses and groups

=head1 SYNOPSIS

    use Email::Address;

    my $winston's = Email::Address::Mailbox.new(
        display-name => 'Winston Smith',
        user         => 'winston.smith',
        host         => 'recdep.minitrue',
        comment      => 'Records Department',
    );
    print $winston's.address; #> winston.smith@recdep.minitrue

    my $julia's = Email::Address.new('Julia', 'julia@ficdep.minitrue');
    print $julias-address.format; #> Julia <julia@ficdep.minitrue>

    my $user's = Email::Address.parse('user <user@oceania>');
    print $user's.host; #> oceania

    my $goldstein's = Email::Address.parse-one('goldstein@brotherhood.oceania');
    print $goldstein's.user; #> goldstein

    my $emails = join ', ',
        '"Windston Smith <winston.smith@recdep.minitrue> (Records Department)',
        'Julia <julia@ficdep.minitrue>',
        ;
    my @addresses = Email::Address.parse();
    # ($winston's, $julia's) = @addresses;

    use Email::Address
        :format-email-addresses, :format-email-groups,
        :parse-email-addresses, :parse-email-groups;

    # Output these three addresses with .format: Phrase <email> (comment)
    my Str $addresses = format-email-addresses($winston's, $julia's, $user's);

    my Str $groups = format-email-groups(
        Brotherhood => ($winston's, $julia's),
        *           => ($users-address),
    );
    say $groups; #> Brotherhood: "Winston Smith" <winston.smith@recdep.minitrue> (Records Department), Julia <julia@ficdep.minitrue>;, user <user@oceania>
    my @groups = Email::Address.parse($groups, :groups);

    use Email::Address :compose-email-address, :split-email-address;

    my ($user, $host) = split-email-address("julia(outer parter)@ficdep.minitrue');
    say $user; #> julia
    say $host; #> ficdep.minitrue

    my $string = compose-email-address('charrington"@"shop', 'thought.police.oceania');
    say $string; #> "charrington\"@\"shop"@thought.police.oceania

=head1 DESCRIPTION

This is an implementation of the L<RFC 5322|https://tools.ietf.org/html/rfc5322>
parser and formatter of email addresses and groups. It parses an input string
from email headers which contain a list of email addresses or a string from
email headers which contain a list of email addresses or groups of email
addresses (like From, To, Cc, Bcc, Reply-To, Sender, etc.). Also it can generate
a string value for those headers from a list of email address objects. This
is backwards compatible with RFC 2822 and RFC 822.

This code has some part that are ported from Perl's Email::Address::XS, but the parser is built directly from the grammar i RFC 5322.

The parser does not attempt to preserve an email address in its original form. Instead, it breaks the email address down into the semantic bits:

=over

=item display-name

This is the human description of the email address. It will be preserved as-is, for the most part. The parser allows for folding whitespace, which will be unfolded, and comments, which will be removed.

=item address

This is the email address itself. It is broken down into a C<local-part> and a C<domain>. Each of these will be preserved as presented (assuming they pass the RFC 5322 parser, which is quite broad in what it accepts). They will have folding whitespace unfolded and comments removed.

=item comment

This is the full comment that has been extracted during parsing. All the comments found in the email address will be concatenated together to fill this field.

=back

No effort will be made to preserve the white space found in non-semantic parts, such as the space between the display name and the address part.

B<NOTE:> As of this writing, the code is not very optimized. It has also not been reviewed for security.

=head1 EXPORTED SUBROUTINES

None of the following functions are exported by default. Each of them can be
requested by using the function name as a named argument. They are all declared
C<our>, so you can use their fully qualified name with C<Email::Address::> put
on the front.

=head2 sub format-email-addresses

    sub format-email-addresses(*@addresses --> Str:D)

For example:

    use Email::Address :format-email-address;

    my $winston's = Email::Address.new(
        display-name => 'Winston Smith',
        address      => 'winston@recdep.minitrue',
    );

    my $julia's = Email::Address.new(
        display-name => 'Julia',
        address      => 'julia@ficdep.minitrue',
    );

    #> "Winstom Smith" <winston@recdep.minitrue>, Julia <julia@ficdep.minitrue
    say format-email-addresses($winston's, $julia's);

Takes a list of email address objects and returns one formatted string of those
email addresses.

=head2 sub format-email-groups

    sub format-email-groups(*@groups --> Str:D)

For example:

    use Email::Address :format-email-groups;

    my $winston's = Email::Address.new(
        display-name => 'Winston Smith',
        address      => 'winston@recdep.minitrue',
    );

    my $julia's = Email::Address.new(
        display-name => 'Julia',
        address      => 'julia@ficdep.minitrue',
    );

    my $user's = Email::Address.new(:address<user@oceania>);

    #> Brotherhood: "Winston Smith" <winston.smith@recdep.minitrue>, Julia <julia@ficdep.minitrue>;, user@oceania
    say format-email-groups(
        'Brotherhood' => ($winston's, $julia's),
        *             => ($user's),
    );

    #> undisclosed-recipients:;
    say format-email-groups('undisclosed-recipients' => ());

Takes a list of pairs and returns a string that outputs the addresses into the named groups. The key of each pair names the group and the value is the email addresses to include in that group (any iterable object will do). If the group name is C<(Nil)> or C<*> (Whatever), then those addresses will be listed in no group. The order the pairs are given will be preserved in the output.

NOTE: Take care with Perl 6 syntax. You will get a compile time error if you try to pass named arguments to this subroutine. For example, both of these will fail:

    #ERR> Unexpected named argument 'Brotherhood' passed
    my $str1 = format-email-groups(Brotherhood => ($winston's, $julia's));

    #ERR> Unexpected named argument 'Brotherhood' passed
    my $str2 = format-email-groups(:Brotherhood($winston's, $julia's));

Anything that looks like a named argument in Perl 6 is passed as a named argument. This function does not take any named arguments, but expects a list of pairs.

=head2 sub parse-email-addresses

    sub parse-email-addresses(Str:D --> Seq)

For example:

    use Email::Address :parse-email-addresses;

    my $str = '"Winston Smith" <winston.smith@recdep.minitrue>, Julia <julia@ficdep.minitrue>, user@oceania';
    my @addresses = parse-email-addresses($str);
    # @addresses now contains three Email::Address objects, one for each address

Parses an input string and returns a sequence of email addresses. The parsing is performed lazily, so if you want to process the whole string at once, you'll want to use the C<eager> function.

=head2 sub parse-email-groups

    sub parse-email-groups(Str:D --> Seq)

For example:

    use Email::Address :parse-email-groups;

    my $str = 'Brotherhood: "Winston Smith" <winston.smith@recdep.minitrue>, Julia <julia@ficdep.minitrue>;, user@oceania, undisclosed-recipients:;';
    my @groups = parse-email-groups($str);
    # @groups now contains a list of pairs, mapping group names to lists of
    # Email::Address objects.

Parses an input string and returns a sequence of pairs. Each pair's key will be the name of the group and each value will be a List of Email::Address objects for the email addresses listed. Groups can point to an empty list (such as would be the case for the "undisclosed-recipients" group in the example above). Email addresses listed without a group will be put into the pair whose key is C<(Nil)>.

=head2 sub compose-address

    sub compose-address(Str:D $user, Str:D $host --> Str:D)

For example:

    use Email::Address :compose-address;

    my $str = compose-address($user, $host);

Takes an unescaped user part and an unescaped host part of an address and returns an escaped address.

=head2 sub split-address

    sub split-address(Str:D $address --> List:D)

For example:

    use Email::Address :split-address;

    my ($user, $host) = split-address($string-addrress);

Given an email address, returns the unescaped user part and the unescaped host part. This returns an empty list if parsing is not possible.

=end pod
