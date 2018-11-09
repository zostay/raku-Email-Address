unit package Email::Address;
use v6;

use Email::Address::Format :ALL;
use Email::Address::Mailbox;
use Email::Address::Parser :parse-email-address;

role Group does Email::Address::Format {
    has Str $.display-name is rw;
    has Email::Address::Mailbox @.mailbox-list;

    method !parse-if-needed(@addresses, :$parser, :$actions) {
        gather for @addresses {
            when Email::Address::Mailbox { .take }
            default {
                my $mailbox = parse-email-address(~$_, :$parser, :$actions, :rule<mailbox>);
                take Email::Address::Mailbox::Parsed.new(
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
        :$parser = Email::Address::RFC5322-Parser,
        :$actions = Email::Address::RFC5322-Actions,
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
        $group ~= @!mailbox-list.map(*.format).join(', ');
        $group ~= ';';
    }

    method Str(--> Str) { self.format }
}

my class Group::Parsed does Group {
    has Str $.original;
}

=begin pod

=head2 Email::Address::Group

The group class represents a list of emails grouped by a name.

=head3 method display-name

    has Str $.display-name is rw

This is the name of the email address group.

=head3 method mailbox-list

    has Email::Address::Mailbox @.mailbox-list

This is the list of mailboxes in the group. Groups cannot be nested.

=head3 method format

    method format(--> Str)
    method Str(--> Str)

This outputs the email address group with the name and all the mailboxes associated with it.

=head2 Email::Address::Group::Parsed

When a group is parsed, this is the actual class returned and it also contains a reference to the original string that was parsed to create the group object.

=head3 method original

    has Str $.original

This is the original string that was parsed to create the email address group.

=end pod
