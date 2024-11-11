# imgur
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

This package provides an unofficial Emacs client for Imgur API.  Currently it
makes the basic functionality available such as authorization, uploading and
deleting in both interactive and non-interactive approach.  On top of that it
provides an option to keep multiple sessions active with functions/commands
named `*-with-session` (otherwise using an automatically created default
session).

## How to:

Clone and install manually, then:

* `imgur-authorize-interactive` (or `imgur-authorize`)
* `imgur-upload-interactive` (or `imgur-upload`)
  * for simpler access `imgur-upload-image-interactive`
* `imgur-delete-interactive` (or `imgur-delete`)

### Check these resources' documentation for more info

* https://status.imgur.com
* https://apidocs.imgur.com
* customization group `imgur`
* `imgur-*-success-func` / `imgur-*-fail-func`

[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
