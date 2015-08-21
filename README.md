# irc-data

This is a Haskell library for parsing, abstracting, and formatting raw
IRC messages, according to [RFC 2812][3].

Documentation on how to install and use the library will come when the
library is somewhat usable.

The library is licensed under the permissive [Apache License][2].

## Building & testing

No Hackage release yet, but here's how to build the development version.

You need [Git][4] and [Stack][5].

```
git clone git://github.com/pharpend/irc-data.git
cd irc-data
stack build --test
```

[1]: https://tools.ietf.org/html/rfc2812
[2]: https://www.apache.org/licenses/LICENSE-2.0
[3]: https://tools.ietf.org/html/rfc2812
[4]: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
[5]: https://github.com/commercialhaskell/stack/wiki/Downloads
