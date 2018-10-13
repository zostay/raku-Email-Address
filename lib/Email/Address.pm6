unit class Email::Address;
use v6;

# no precompilation;
# use Grammar::Tracer;
use Grammar::ABNF;

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
    token angle-addr  { <cfws>? '<' <addr-spec> '>' <cfws>? |
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
    token domain-literal { <cfws>? '[' [<fws>? <dtext>]* <fws> ']' }

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
    token atom        { <cfws>? <atext>+ <cfws>? }

    # dot-atom-text   =   1*atext *("." 1*atext)\r
    token dot-atom-text { <atext>+ [ '.' <atext>+ ]* }

    # dot-atom        =   [CFWS] dot-atom-text [CFWS]\r
    token dot-atom    { <cfws>? <dot-atom-text> <cfws>? }

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
    token comment     { '(' [ <fws>? <ccontent> ]* <fws>? ')' }

    # CFWS            =   (1*([FWS] comment) [FWS]) / FWS\r
    token cfws        { [ [ <fws>? <comment> ]+ <fws>? ] | <fws> }

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
    token quoted-string { <cfws>?
                          <dquote> [ <fws>? <qcontent> ]* <fws>? <dquote>
                          <cfws>? }

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

    # obs-body        =   *((*LF *CR *((%d0 / text) *LF *CR)) / CRLF)\r
    token obs-body    { [ [ <lf>* <cr>* [ [ \0 | <text> ] <lf>* <cr>* ]* ] | <crlf> ]* }

    # obs-phrase      =   word *(word / "." / CFWS)\r
    token obs-phrase  { <word> [ <word> | '.' | <cfws> ]* }

    # obs-phrase-list =   [phrase / CFWS] *("," [phrase / CFWS])\r
    token obs-phrase-list { [ <phrase> | <cfws> ]? [ ',' [ <phrase> | <cfws> ]? ]* }

    # quoted-pair     =   ("\\" (VCHAR / WSP)) / obs-qp\r
    token quoted-pair { [ '\\' [ <vchar> | <wsp> ] ] | <obs-qp> }

    # obs-angle-addr  =   [CFWS] "<" obs-route addr-spec ">" [CFWS]
    token obs-angle-addr { <cfws>? '<' <obs-route> <addr-spec> '>' <cfws>? }

    # obs-route       =   obs-domain-list ":"
    token obs-route   { <obs-domain-list> ':' }

    # obs-domain-list =   *(CFWS / ",") "@" domain
    #                     *("," [CFWS] ["@" domain])
    token obs-domain-list { [ <cfws> | ',' ]* '@' <domain>
                            [ ',' <cfws>? [ '@' <domain> ]? ]* }

    # obs-mbox-list   =   *([CFWS] ",") mailbox *("," [mailbox / CFWS])
    token obs-mbox-list { [ <cfws>? ',' ]* <mailbox> [ ',' [ <mailbox> | <cfws> ]? ]* }

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
}

sub format-email-addresses(*@addresses --> Str) is export(:format-email-addresses) {
    format-email-groups((Nil) => @addresses);
}

sub format-email-groups(*@groups --> Str) is export(:format-email-groups) {
    ...
}

sub parse-email-addresses(Str:D $addresses --> Seq) is export(:parse-email-addresses) {
    ...
}

sub parse-email-groups(Str:D $groups --> Seq) is export(:parse-email-groups) {
    ...
}

sub compose-address(Str $mailbox, Str $domain --> Str) is export(:compose-address) {
    ...
}

sub split-address(Str $input --> List) is export(:split-address) {
    ...
}

class AddrSpec {
    has Str $.user is rw;
    has Str $.host is rw;

    method Str(--> Str:D) { compose-address($!user, $!host) }
}

subset CommentStr of Str where {
    my regex balanced-parens { <-[()]>+ | '()'+ | '(' ~ ')' <balanced-parens> }
    /^ <balanced-parens> $/
};

has Str $.phrase is rw;
has AddrSpec $.address is rw;
has CommentStr $.comment is rw;

has Str $!original;
has Bool $!invalid;

multi method new(::?CLASS:U:
    Str $phrase,
    Str $address,
    Str $comment,
) {
    self.bless(:$phrase, :$address, :$comment);
}

submethod BUILD(
    :$!phrase,
    :$!address,
    :$!comment,
    :$user,
    :$host,
) {
    die "When constructing Email::Address, you may pass either address or host and user, but not both."
        if $!address.defined && ($user.defined || $host.defined);

    # address prevents user/host from being set
    $!address = AddrSpec.new(:$user, :$host) without $!address;
}

method user(--> Str) is rw { return-rw $!address.user }
method host(--> Str) is rw { return-rw $!address.host }
method original(--> Str) { $!original }

method set-address(Str:D $address) {
    ($!address.user, $!address.host) = split-address($address);
    $!address.user = Nil without $!address.host;
    $!address.host = Nil without $!address.user;
}

method name(--> Str) {
    with $!phrase { return $!phrase if $!phrase.chars }
    with $!comment { return $!comment if $!comment.chars }
    with $.user { return $.user }
    '';
}

method Str(--> Str) { self.format }

method parse(::?CLASS:U: Str:D $string --> Seq) {
    parse-email-addresses($string, self);
}

method parse-bare-address(::?CLASS:U: Str:D $address --> Email::Address) {
    self.new(:$address);
}

method format(::?CLASS:D: --> Str:D) {
    format-email-addresses(self);
}

method is-valid(::?CLASS:D: --> Bool:D) {
    $.user.defined && $.host.defined && $.host.chars;
}

=begin pod

=head1 NAME

Email::Address - parse and format RFC 5322 email addresses and groups

=head1 SYNOPSIS

    use Email::Address;
    my $winston's = Email::Address.new(
        phrase  => 'Winston Smith',
        user    => 'winston.smith',
        host    => 'recdep.minitrue',
        comment => 'Records Department',
    );
    print $winston's.address; #> winston.smith@recdep.minitrue

    my $julia's = Email::Address.new('Julia', 'julia@ficdep.minitrue');
    print $julias-address.format; #> Julia <julia@ficdep.minitrue>

    my $user's = Email::Address.parse('user <user@oceania>');
    print $user's.host; #> oceania

    my $goldstein's = Email::Address.parse('goldstein@brotherhood.oceania', :bare);
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

This is a port of the Email::Address::XS module into pure Perl6, this includes
porting the C code that was used from the Dovecot IMAP project that does the
actual parsing. The code was converted to idiomatic Perl 6, though, so do not
expect the interface to be identical to the Perl 5 counterpart.

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
        phrase  => 'Winston Smith',
        address => 'winston@recdep.minitrue',
    );

    my $julia's = Email::Address.new(
        phrase  => 'Julia',
        address => 'julia@ficdep.minitrue',
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
        phrase  => 'Winston Smith',
        address => 'winston@recdep.minitrue',
    );

    my $julia's = Email::Address.new(
        phrase  => 'Julia',
        address => 'julia@ficdep.minitrue',
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
