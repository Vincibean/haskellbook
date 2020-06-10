# Chapter 1
## Intermission: Equivalence Exercises
We’ll give you a lambda expression. Keeping in mind both alpha equivalence and how multiple heads are nested, choose an answer that is equivalent to the listed lambda term

#### 1 𝜆𝑥𝑦.𝑥𝑧
b) 𝜆𝑚𝑛.𝑚𝑧
#### 2. 𝜆𝑥𝑦.𝑥𝑥𝑦
c) 𝜆𝑎(𝜆𝑏.𝑎𝑎𝑏)
#### 3. 𝜆𝑥𝑦𝑧.𝑧𝑥 
b) 𝜆𝑡𝑜𝑠.𝑠`

## Chapter Exercises
### Combinators
Determine if each of the following are combinators or not.

Since "A combinator is a lambda term with no free variables":

#### 1. 𝜆𝑥.𝑥𝑥𝑥
This is a combinator: every variable is bound.
#### 2. 𝜆𝑥𝑦.𝑧𝑥
This isn't a combinator: z isn't bound (while the fact that y is defined but not used is inconsequential).
#### 3. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥)
This is a combinator: every variable is bound.
#### 4. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦)
This is a combinator: every variable is bound.
#### 5. 𝜆𝑥𝑦.𝑥𝑦(𝑧𝑥𝑦)
This isn't a combinator: z isn't bound.

### Normal form or diverge?
Determine if each of the following can be reduced to a normal form or if they diverge.

#### 1. 𝜆𝑥.𝑥𝑥𝑥
Can't be reduced more; it doesn't diverge, so it's in normal form.
#### 2. (𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦)
Becomes `(𝜆[z := 𝜆𝑦.𝑦𝑦].𝑧𝑧)` => (𝜆𝑦.𝑦𝑦)(𝜆𝑦.𝑦𝑦)

We're back to where we started. This doesn't converge to normal form, so it diverges.

#### 3. (𝜆𝑥.𝑥𝑥𝑥)𝑧
Becomes: (𝜆[𝑥 := z].𝑥𝑥𝑥) => zzz

Can't be reduced more; it doesn't diverge, so it's in normal form.

### Beta reduce
Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.

#### 1. (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)
```
(𝜆𝑎.𝜆𝑏.𝜆𝑐.𝑐𝑏𝑎)(𝑧)(𝑧)(𝜆𝑤.𝜆𝑣.𝑤)
(𝜆[𝑎 := z].𝜆𝑏.𝜆𝑐.𝑐𝑏𝑎)(𝑧)(𝜆𝑤.𝜆𝑣.𝑤)
(𝜆𝑏.𝜆𝑐.𝑐𝑏z)(𝑧)(𝜆𝑤.𝜆𝑣.𝑤)
(𝜆[𝑏 := z].𝜆𝑐.𝑐𝑏z)(𝜆𝑤.𝜆𝑣.𝑤)
(𝜆𝑐.𝑐zz)(𝜆𝑤.𝜆𝑣.𝑤)
(𝜆[𝑐 := (𝜆𝑤.𝜆𝑣.𝑤)].𝑐zz)
(𝜆𝑤.𝜆𝑣.𝑤)zz
(𝜆𝑤.𝜆𝑣.𝑤)(z)(z)
(𝜆[𝑤 := z].𝜆𝑣.𝑤)(z)
(𝜆𝑣.z)(z)
(𝜆[𝑣 := z].z)
z
```

#### 2. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏
```
(𝜆[x := (𝜆𝑎.𝑎)].𝜆𝑦.𝑥𝑦𝑦)𝑏
(𝜆𝑦.(𝜆𝑎.𝑎)𝑦𝑦)𝑏
(𝜆[𝑦 := 𝑏].(𝜆𝑎.𝑎)𝑦𝑦)
(𝜆𝑎.𝑎)bb
(𝜆𝑎.𝑎)(b)(b)
(𝜆[𝑎 := b].𝑎)(b)
(b)(b)
bb
```

#### 3. (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)
```
(𝜆[𝑦 := (𝜆𝑥.𝑥𝑥)].𝑦)(𝜆𝑧.𝑧𝑞)
(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)
(𝜆[𝑥 := (𝜆𝑧.𝑧𝑞)].𝑥𝑥)
(𝜆𝑧.𝑧𝑞)(𝜆𝑧.𝑧𝑞)
(𝜆[𝑧 := (𝜆𝑧.𝑧𝑞)].𝑧𝑞)
(𝜆𝑧.𝑧𝑞)𝑞
(𝜆[𝑧 := 𝑞].𝑧𝑞)
𝑞𝑞
```

#### 4. (𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦) Hint: alpha equivalence.
```
(𝜆[𝑧 := (𝜆𝑧.𝑧𝑧)].𝑧)(𝜆𝑧.𝑧𝑦) -> We can jump this passage by noticing that the first function is the identity function: the output is the input
(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)
(𝜆[𝑧 := (𝜆𝑧.𝑧𝑦)].𝑧𝑧)
(𝜆𝑧.𝑧𝑦)(𝜆𝑧.𝑧𝑦)
(𝜆[𝑧 := (𝜆𝑧.𝑧𝑦)].𝑧𝑦)
(𝜆𝑧.𝑧𝑦)𝑦
(𝜆[𝑧 := 𝑦].𝑧𝑦)
𝑦𝑦
```

#### 5. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑦.𝑦)𝑦
```
(𝜆[𝑥 := (𝜆𝑦.𝑦)].𝜆𝑦.𝑥𝑦𝑦)𝑦
(𝜆𝑦.(𝜆𝑦.𝑦)𝑦𝑦)𝑦
(𝜆[𝑦 := y].(𝜆𝑦.𝑦)𝑦𝑦)
(𝜆𝑦.𝑦)𝑦𝑦
(𝜆𝑦.𝑦)(𝑦)(𝑦) -> identity function again
(𝑦)(𝑦)
𝑦𝑦
```

#### 6. (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐
```
(𝜆[𝑎 := (𝜆𝑏.𝑏𝑎)].𝑎𝑎)𝑐
(𝜆𝑏.𝑏𝑎)(𝜆𝑏.𝑏𝑎)𝑐
(𝜆[𝑏 := (𝜆𝑏.𝑏𝑎)].𝑏𝑎)𝑐
((𝜆𝑏.𝑏𝑎)(𝑎))𝑐
((𝜆[𝑏 := 𝑎].𝑏𝑎))𝑐
(𝑎𝑎)𝑐
𝑎𝑎𝑐
```

#### 7. (𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)
```
(𝜆𝑥.𝜆𝑦.𝜆𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)
(𝜆𝑥.𝜆𝑦.𝜆𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧')(𝜆𝑥.𝑎)
(𝜆[𝑥 := (𝜆𝑥.𝑧')].𝜆𝑦.𝜆𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑎)
(𝜆𝑦.𝜆𝑧.(𝜆𝑥.𝑧')𝑧(𝑦𝑧))(𝜆𝑥.𝑎)
(𝜆[𝑦 := (𝜆𝑥.𝑎)].𝜆𝑧.(𝜆𝑥.𝑧')𝑧(𝑦𝑧))
(𝜆𝑧.(𝜆𝑥.𝑧')𝑧((𝜆𝑥.𝑎)𝑧))
(𝜆𝑧.(𝜆[𝑥 := 𝑧].𝑧')((𝜆𝑥.𝑎)𝑧))
(𝜆𝑧.𝑧'((𝜆𝑥.𝑎)𝑧))
(𝜆𝑧.𝑧'((𝜆[𝑥 := 𝑧].𝑎)))
(𝜆𝑧.𝑧'((𝑎)))
𝜆𝑧.𝑧'𝑎
```