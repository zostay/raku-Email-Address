unit class Email::Address;
use v6;

use Email::Address::Group;
use Email::Address::Mailbox;
use Email::Address::Parser :parse-email-address;

class GLOBAL::X::Email::Address is Exception { }

class GLOBAL::X::Email::Address::Syntax is Exception {
    has $.part;
    has $.input;

    method message(--> Str) {
        qq[Syntax error in string "{$!input.trans('"' => '\"')}" while parsing $!part];
    }
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
    gather for parse-email-address($str, :$parser, :$actions, :rule<mailbox-list>) {
        take build-mailbox($_);
    }
}

multi method parse(::?CLASS:U: Str $str, :$groups!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Seq) {
    gather for parse-email-address($str, :$parser, :$actions, :rule<group-list>) {
        take build-group($_);
    }
}

multi method parse(::?CLASS:U: Str $str, :$addresses!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Seq) {
    gather for parse-email-address($str, :$parser, :$actions, :rule<address-list>) {
        when so .<type> eq 'mailbox' { take build-mailbox($_) }
        when so .<type> eq 'group' { take build-group($_) }
    }
}

multi method parse-one(::?CLASS:U: Str $str, :$mailbox!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Mailbox) {
    build-mailbox($_)
        given parse-email-address($str, :$parser, :$actions, :rule<mailbox>);
}

multi method parse-one(::?CLASS:U: Str $str, :$group!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Group) {
    build-group($_)
        given parse-email-address($str, :$parser, :$actions, :rule<group>);
}

multi method parse-one(::?CLASS:U: Str $str, :$address!, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> Any) {
    given parse-email-address($str, :$parser, :$actions, :rule<address>) {
        when so .<type> eq 'mailbox' { take build-mailbox($_) }
        when so .<type> eq 'group' { take build-group($_) }
    }
}


method format(::?CLASS:U: *@addresses --> Str) {
    join ', ', gather for @addresses -> $_ is copy {
        when Format { take .format }
        when Callable { $_ = .arity == 1 ?? .(Nil) !! .(); proceed }
        when Pair {
            my ($display-name, @mailboxes) = .kv;
            take Group.new(:$display-name, :@mailboxes);
        }
        default {
            die 'unknown object sent to .format()';
        }
    }
}

multi method compose(::?CLASS:U: Str $local-part, Str $domain --> Str) {
    AddrSpec.new(:$local-part, :$domain).format
}

multi method split(::?CLASS:U: Str $address, :$parser = RFC5322-Parser, :$actions = RFC5322-Actions --> List) {
    (.<local-part>, .<domain>)
        given parse-email-address($address, :$parser, :$actions, :rule<addr-spec>);
}

# =begin pod
#
# =head1 NAME
#
# Email::Address - parse and format RFC 5322 email addresses and groups
#
# =head1 SYNOPSIS
#
#     use Email::Address;
#
#     my $peyton's = Email::Address::Mailbox.new(
#         display-name => 'Peyton Randalf',
#         local-part   => 'peyton.randalf',
#         domain       => 'example.com',
#         comment      => 'Virginia House of Burgesses',
#     );
#     print $peyton's.address; #> petyon.randalf@example.com
#
#     my $henry's = Email::Address::Mailbox.new('Henry', 'henry@example.com');
#     print $henry's.format; #> Henry <henry@example.com>
#
#     my $andrew's = Email::Address.parse-one('adams <a.adams@example.com>', :mailbox);
#     print $andrew's.domain; #> example.com
#
#     my $laurens's = Email::Address.parse-one('laurens@example.com');
#     print $laurens's.local-part; #> laurens
#
#     my $emails = join ', ',
#         '"John Jay" <john.jay@example.com> (New York Supreme Court)',
#         'Huntington <shunting@example.com>',
#         ;
#     my @mailboxes = Email::Address.parse($emails, :mailboxes);
#     # my ($jay's, $samuel's) = @mailboxes;
#
#     # Output these three addresses with .format: Phrase <email> (comment)
#     my Str $addresses = Email::Address.format: $peyton's, $henry's, $john's);
#
#     my Str $addresses = Email::Address.format:
#         'Presidents' => ($peyton's, $henry's),
#         *            => ($andrew's),
#     );
#     say $addresses; #> Presidents: "Peyton Randalf" <peyton.randalf@example.com> (Virginia House of Burgesses), Henry <henry@example.com>;, adams <a.adams@example.com>
#     my @addresses = Email::Address.parse($groups, :addresses);
#
#     my ($local-part, $domain) = Email::Address.split("henry(2nd pres.)@example.com');
#     say $local-part; #> henry
#     say $domain;     #> example.com
#
#     my $string = Email::Address.compose('t"@"mkean', 'example.com');
#     say $string; #> "t\"@\"mkean"@example.com

=begin pod

=head1 NAME

Email::Address - parse and format RFC 5322 email addresses and groups

=head1 SYNOPSIS

    use Email::Address;

    my $to-header = q:to/END_OF_TO/;
    Presidents: "Peyton Randalf" <peyton
     .randalf@example.com> (Virginia House of
     Burgesses), Henry <henry@example.com>;,
     adams <a.adams@example.com>
    END_OF_TO

    my @to = Email::Address.parse($to-header, :addresses);
    for @to {
        when Email::Address::Group {
            say "Group: ", .display-name;
        }
        when Email::Address::Mailbox {
            say "Mailbox: ", .display-name;
        }
    }

    say Email::Address.format(@to);

    my $email-address = '"John Jay" <john.jay@example.com>';
    my Email::Address::Mailbox $john-jay
        = Email::Address.parse-one($email-address, :mailbox);

    say Email::Address.format(
        'Presidents' => (
            '"Peyton Randalf" <peyton.randalf@example.com> (Virginia House of Burgesses)',
            $john-jay,
        ),
        'Henry <henry@example.com>',
    );

=head1 DESCRIPTION

This is an implementation of the L<RFC 5322|https://tools.ietf.org/html/rfc5322>
parser and formatter of email addresses and groups. It can parse  an input
string from email headers which contain a list of email addresses or a string
from email headers which contain a list of email addresses and groups of email
addresses (like From, To, Cc, Bcc, Reply-To, Sender, etc.). Also it can generate
a string value for those headers from a list of email address objects. This is
backwards compatible with RFC 2822 and RFC 822.

This code has some parts that are ported from Perl's Email::Address::XS, but the parser is built directly from the grammar in RFC 5322. This does not adhere in any way to the API in Email::Address or Email::Address::XS of Perl 5 as there were some legacy oddities I felt it best to leave behind.

This class is generally used without constructing an instance, but you can construct an instance too if you prefer. There's no real advantage to doing that though.

=head1 METHODS

=head2 method parse

    multi method parse(Str $str, :$mailboxes!, :$parser, :$actions --> Seq)
    multi method parse(Str $str, :$groups! :$parser, :$actions --> Seq)
    multi method parse(Str $str, :$addresses!, :$parser, :$actions --> Seq)

The parse methods take a string and return zero or more email address objects. When calling the parse method, you must provide an adverb to specify the kind of parsing to perform:

=defn C<:mailboxes>
The parser will parse this as a list of mailboxes and return a sequence of C<Email::Address::Mailbox> objects.

=defn C<:groups>
The parser will parse this as a list of groups and return a sequence of C<Email::Address::Group> objects.

=defn C<:addresses>
The parser will parse this as an address list, which may contain a combination of mailboxes and groups. The sequence returned may contain C<Email::Address::Group> and C<Email::Address::Mailbox> objects.

If the given string cannot be parsed, an C<X::Email::Address> exception will be thrown.

=head2 method parse-one

    multi method parse-one(Str $str, :$mailbox!, :$parser, :$actions --> Seq)
    multi method parse-one(Str $str, :$group! :$parser, :$actions --> Seq)
    multi method parse-one(Str $str, :$address!, :$parser, :$actions --> Seq)

The parse-one methods take a string and return exactly one email address object. When calling this method, you must provide an adverb to specify the kind of parsing to perform:

=defn C<:mailbox>
The parser will parse this as a single mailbox and return a C<Email::Address::Mailbox>

=defn C<:group>
The parser will parse this as a single group and return a C<Email::Address::Group>

=defn C<:address>
The parser will parse this as a single email address or group and will return either a C<Email::Address::Group> or C<Email::Address::Mailbox>.

If the given string does not match a single email address, an C<X::Email::Address> exception will be thrown.

=head2 method format

    method format(*@addresses --> Str)

Given a list of arguments, this method will return a string suitable for inserting into an RFC 5322 formatted email header. Each item passed may be an C<Email::Address::Mailbox>, C<Email::Address::AddrSpec>, C<Email::Address::Group>, C<Pair>, or a C<Str>.

The email address objects will be formatted using their C<.format> method.

Pairs will be treated as lightweight groups. The key will be treated as the group display-name and the value may be a list of zero or more addresses to put into the group. (Internally, this will create a group, which is formatted.) The values may be passed as either mailbox objects or strings. When given as strings, they will be parsed as mailboxes, which can trigger a C<X::Email::Address> exception if a mailbox cannot parsed.

Sometimes, it is nice to pass everything as a list of pairs, but you still want to have some mailbox addresses outside of a group. This can be done by using Whatever as the key in the pair. For example.

    my Str $addresses = Email::Address.format:
        'Presidents' => ($peyton's, $henry's),
        *            => ($andrew's,),
    );

The values set in the last pair will be output outside of any group.

Strings will be treated as email addresses. They will be parsed and then formatted. If parsing occurs and the email addresses given are not valid, the method will thrown an C<X::Email::Address> exception.

Other objects passed will trigger an exception.

=head2 method compose

    method compose(Str $local-part, Str $domain --> Str)

This is a quick helper for combinging a local-part and domain part into a "local-part@domain" string.

=head2 method split

    method split(Str $address, :$parser, :$actions --> List)

This method takes an addr-spec (bare email address) and returns the local-part and domain part. For example:

    my ($local-part, $domain) = Email::Address.split("foo@example.com");
    say $local-part; #> foo
    say $domain;     #> example.com

This is aimed at convenience, not an optimization and does perform a full parse of the addr-spec (though, it skips the object construction step always performed by C<method parse> and C<method parse-one>).

=end pod
