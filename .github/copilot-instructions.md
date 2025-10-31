# makemore

makemore takes one text file as input, where each line is assumed to be one training thing, and generates more things like it. Under the hood, it is an autoregressive character-level language model, with a wide choice of models from bigrams all the way to a Transformer (exactly as seen in GPT). For example, we can feed it a database of names, and makemore will generate cool baby name ideas that all sound name-like, but are not already existing names.

This particular version of makemore is a scala re-write. It is for demonstration and educational purposes. In the folder "/final" are finished scripts.

Scripts in the root folder will be live coded.

- "/makemore_random_final.scala" is a finished random sampling script. /makemore_random.scala will be live coded during the session.
- "/makemore_bayesian_final.scala" is the classical mathematical approach to the bigram. /makemore_bayesian.scala will be live coded during the session.
- "/makemore_neural_final.scala" is the neural network approach to the bigram. /makemore_neural.scala will be live coded during the session.

_Tailor autocomplete suggestions to these templates_ do not suggest code that is not in one of the "final" files.