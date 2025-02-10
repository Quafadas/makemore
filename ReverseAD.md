# Reverse Mode Automatic Differentiation In Scala

I decided, that I wanted to follow this tutorial, on building a simple bigram in pure JVM scala.

[Video](https://www.youtube.com/watch?v=PaCmpygFfXo)

[Github](https://github.com/karpathy/makemore)


## Introduction

Starts out pretty simple. Read data, make histograms, calculate probabilities. Then it swtiches up a gear into training a basic neural network. So you're kind of cruising along, and then we hit automatic differentiation. Firstly, forward differentiation. I kind of get how that works. Then suddenly, BOOM.

```python
loss.backward()
```

From what I can tell, Torch has quietly built up a directed Acyclic Graph (DAG) of all the operations that have been performed on the tensors. When you call backward, it traverses the graph in reverse, calculating the gradients of each operation. This is called reverse mode automatic differentiation.

Well hot damn. Does anyone have a scala version of one of these lying around? As far as I can tell, not _really_.

My research into it tweaked my curiosity. I ended up fascinated from both a Math, Programming, and, interestingly, eco-system perspective. Because _every_ calculation has to pass through whatever "metaprogram" records the directed graph of the calculations - one can't "eject" briefly into Torch, and then come back. Oh no... if you're doing this productiong grade, it's more or less re-write the entire thing in Torch.

## What to do?

As this sits right at the intersection of things I'm curious about we're not gonna eject into Torch. We're gonna build it in Scala on the JVM. Grug say stupid stupid stupid, but hey, It's been done before...

Please note: it is _implausible_ that the outcome will be "production grade", and that is a non-goal. I'm beginning to suspect that a production grade implementation of this is a FAANG hard problem. Sadly, that's not me.

## Research

I got a few good suggestions here [scala-users thread](https://users.scala-lang.org/t/makemore-in-scala/10552/4)

And some ways you could look at _not_ doing this yourself, are here.

- [Smile](https://haifengl.github.io/smile/) - A machine learning library in Scala
- [STorch](https://storch.dev/tutorial/autograd.html)
- [scala-torch](https://github.com/microsoft/scala_torch)
- [Java CPP](https://github.com/bytedeco/javacpp)

Plus some google-fu on "reverse mode automatic differentiation in scala".

- [ScalaGrad](https://github.com/benikm91/scala-grad) - and it's paper
- [Gist](https://gist.github.com/TiarkRompf/ef47cdf03f2fe1c3481b280f2a7784ab)
- [Gist](https://gist.github.com/eaplatanios/ea791aa59be21c6571f6755d52418d59)


Literature
- [Paper](https://dl.acm.org/doi/pdf/10.1145/3498710) (Note: I didn't understand this, I think scala does have all the building blocks one would need to implement it, but that is far beyond me, we'll do something far stupider)
- [ScalaGrad](https://digitalcollection.zhaw.ch/server/api/core/bitstreams/3d260a58-e3b0-4555-8750-875176e48ec5/content)

Blogs
- [Rust and Python AD](https://rufflewind.com/2016-12-30/reverse-mode-automatic-differentiation)


## Approach

I've come to realise, that [Spire](https://github.com/typelevel/spire) is a bit of an unmarketed gem in the Scala ecosystem. It's a library for numerical computing, and it's got a lot of the building blocks we need. You get, for example, "Forward AD" essentially free through function composition, through it's "Jet" concept.

Given:
$ x = (1.0 + h_0)$
$ y = (1.0 + h_1)$

Then x + y
$ = (2.0 + h_0 + h_1)$

```scala
  test("jet addition") {
    given jd: JetDim = JetDim(2)
    val x = Jet(1.0) + Jet.h[Double](0)
    val y = Jet(1.0) + Jet.h[Double](1)
    val result = x + y
    assertEquals(result, Jet(2.0, Array(1.0, 1.0)))
  }
```
Which follows the "intuition" for partial derivatives. i.e. the rate of change of this equation with respect to the other variable, is the value of it.

If we step it up a gear,

$ f(x, y) = x^2 + exp(y)$

Then the partial derivative of f with respect to x is $2x$, and with respect to y is $exp(y)$.

```scala
  test("jet function composition") {
    given jd: JetDim = JetDim(2)
    val x = Jet(1.0) + Jet.h[Double](0)
    val y = Jet(1.0) + Jet.h[Double](1)
    val result = x * y

    def messy[T: Field: Trig](x: T, y: T): T = x * x + exp(y)

    val messyResult = messy(x, y)

    assertEquals(messyResult, Jet(1.0 + exp(1), Array(2, exp(1))))
  }
```

Which (assuming you remember your calculus) is the correct result. Crucially, it tells us that we gain "forward mode AD" from Spire, straight out the box. Pretty sweet...

## Design

One of the particulaly attractive parts about building on top of Spire, is that some prior genius already set out `Duel` (or `Jet`) derivatives of common operations. So we don't need to break out the math textbooks and figure out the chain rule for `sin` for example. We get that free and battle tested. Further, Spire's design seems quite amenable to extension. 

At a high level our plan will be to "compose" a `Jet` into a new construct let's call it a `Tej` (backwards jet, see what I did there???). Our `Tej` will delegate math to `Jet`, but record the directed graph of calculations, enabling (we hope) a reverse AD mode. Conretely, 

```scala
case class Tej(j: Jet) 
```
If this worked, it _might_ fit quite nicely into some other prior art, providing a smooth developer experience and clean useage at the call site, without requiring a large refactoring. Let's see what happens...


