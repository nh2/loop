0.3.0
-----

* Changed behaviour of `numLoop` to do nothing when not `start <= end`.
  That means it no longer wraps around for bounded types (e.g. Int),
  and no longer loops forever for unbounded types (e.g. Integer).
  See #3.

0.1.0
-----

* Initial version.
