Spanish-Conjugation
===================

The scope of this repository has expanded to Spanish practice and spaced repetition in general,
but conjugation remains the main focus.

SpanishConjugation.hs
---------------------

A program that conjugates verbs in Spanish and tests the user via spaced repetition.

What's unique about this program is that it conjugates verbs using the same procedure as a person.
I'm using it in order to understand and improve my own mental conjugation process.
This module includes the conjugation rules and the necessary IO.

SpanishConjugationData.hs
-------------------------

Stores data associated with individual verbs. This data is represented by functions
because it's a closer analogue to how humans think than using tables.

SpacedRepetition.hs
-------------------

Defines the Card class and implements a serialized spaced repetition game for members of the class.
Cards have two necessary attributes: a unique ID, and an IO action which returns a Bool.

SpanishVocab.hs
---------------

A small implementation of the Card class for reviewing vocab.

ParseSpanish.hs
---------------

Defines the parseSpanish function, which translates "'" into an accent on the previous character
and "n~" into ñ.

ImmediateIO.hs
--------------

Defines functions for unbuffered IO.
