erc-hipchatify.el
=================

This makes chatting via BitlBee to a HipChat server a little more
palatable. Note worth features:

* download HipChat emoticons (eg. (awyeah))
* provide company backend for emoticons and @ completions
* render html and images (png/jpg/gif/svg) via shr
* notify @here mentions

Usage
-----

.. code:: lisp

    (require 'erc-hipchatify)
    (add-to-list 'erc-modules 'hipchatify)
    (erc-update-modules)

Credits
-------

-  `Sean Farley <https://bitbucket.org/seanfarley>`__
