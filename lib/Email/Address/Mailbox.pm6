unit package Email::Address;
use v6;

use Email::Address::Parser :parse-email-address;
use Email::Address::Format :ALL;

role AddrSpec does Format {
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

role Mailbox does Format {
    has Str $.display-name is rw;
    has AddrSpec $.address is rw;
    has CommentStr $.comment is rw;

    method local-part(--> Str) is rw { return-rw $!address.local-part }
    method domain(--> Str) is rw { return-rw $!address.domain }

    method set-address(
        Str:D $address,
        :$parser = Email::Address::RFC5322-Parser,
        :$actions = Email::Address::RFC5322-Actions,
        --> AddrSpec
    ) {
        my %addr-spec = parse-email-address($address, :$parser, :$actions, :rule<addr-spec>);
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

=begin pod

=head2 Email::Address::Mailbox

This class contains a email address representing a single mailbox. This is what most people think of when they here the term "email address."

=head3 method display-name

    has Str $.display-name is rw

This is the name associated with the mailbox address. In RFC 5322 and earlier RFCs this is the word or quoted words that come before the email address.

=head3 method address

    has Email::Address::AddrSpec $.address is rw

This is the email address itself with the at-sign in it. In a mailbox address, this may be embedded inside of arrow-brackets.

=head3 method comment

    has Email::Address::CommentStr $.comment is rw

This is the comment associated with this address. Comments may be inserted almost anywhere in the email address and are basically any string inside parenthesis. The only requirement is that any nested opening or closing parenthesis mark in the comment itself must have matching closing or opening parenthesis, respectively. When parsing all comments are concatenated together to form one long comment. When formatting, all the comments will be put together as a single comment added after the email address.

=head3 method local-part

    method local-part(--> Str) is rw

This is a shortcut for getting at the C<.local-part> accessor of C<method address>.

=head3 method domain

    method domain(--> Str) is rw

This is a shortcut for getting at the C<.domain> accessor of C<method address>.

=head3 method set-address

    method set-address(Str $address, :$parser, :$actions --> Email::Address::AddrSpec)

This method will parse the given C<$address> and replace the C<method address>.

=head3 method guess-name

    method guess-name(--> Str)

This ethod can be helpful for trying to get a name to associate with an email address. It will return the first of the following that are present in the email address:

=defn display-name
The display name given in the mailbox address is almost always the name of the owner or group for the name as that is the intended purpose of this field.

=defn comment
Sometimes the name for the email address will be in the comment.

=defn local-part
Failing the above, this is always present in a mailbox address and is the fallback for guessing the name.

=head3 method format

    method format(--> Str)
    method Str(--> Str)

Renders the email address in a standard form that follows RFC 5322 whenever possible.

=head2 Email::Address::Mailbox::Parsed

When the parser is used to parse an email address, this object is returned, which also contains a reference to the original parsed string.

=head3 method original

    has Str $.original

This is the original string that was parsed to create the mailbox address.
=head2 Email::Address::AddrSpec

The actual email address part of a mailbox is represented by an addr-spec. This contains the local-part and domain of the email address, which can be used for routing.

=head3 method local-part

    has Str $.local-part is rw

This is the local part of an email address, often being the user name of the recipient. It is the part before at-sign.

=head3 method domain

    has Str $.domain is rw


This is the domain part of an email address. This is generally a regular domain name address, but might also be an IP address or some other sort of name.

=head3 method format

    method format(--> Str)
    method Str(--> Str)

This will output the addr-spec in the RFC 5322 form or an obsolete form if necessary.

=head2 Email::Address::AddrSpec::Parsed

When an addr-spec is parsed, this class is returned, which also contains a reference back to the original parsed string.

=head3 method original

    has Str $.original

This is the original string that was parsed to produce an addr-spec.

=end pod
