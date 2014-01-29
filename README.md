## 鬼ラム・インベーダ
This is Oni, the next version. Focus is on design, API, tests and documentation.

#### Bindings
The binding list is an important `proplist()` that is passed around a few places. It should contain the following items:

*   `this` is the object on which the verb was found
*   `player` executing the command
*   `caller`, usually `player`
*   `verb` is the first token of the command
*   `argstr`, everything after `verb`
*   `dobjstr` is the direct object string if any
*   `dobj` is the odirect object if any was found
*   `prepstr` is the preposition if any was found
*   `iobjstr` is the indirect object string if any
*   `iobj` is the indirect object if any was found