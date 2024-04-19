In the next chapter, we will provide more details about certain UI aspects. 



### Extra Spec behavior

The integration of Commander into Spec allows one to access features that are only available for menu items,

- `beDisabledWhenCantBeRun`.  Julien help please. How we know that it cannot be run? How do I specify the condition? Can I specify the condition?  Pay attention  that this is static feature in the sense that it is evaluated at the command creation. Is it true?
- `beHiddenWhenCantBeRun` Julien help please. How we know that it cannot be run? How do I specify the condition? Can I specify the condition?  Pay attention  that this is static feature.


- `beDisplayedOnRightSide` and `beDisplayedOnLeftSide`. Julien help please the right side of what?


- Contexts can be dynamic. If you want your command to work on a context that will change at execution, pass a block as argument of the `context:` message.
-  Dynamic name and description. Commander allows you to customize the name and description of a command. 
The idea is to let you provide description or name that take advantage of the context in which they are used.
- Toolbar. Commands can also be turned into a toolbar using the message `asToolbarPresenter` sent to a group of commands.

### Registration and navigation

Commands are often grouped together to act as menu groups. 
In the previous chapter we show that a group is structured as a composite tree of groups and commands. 
Adding elements to such composite is done via  the messages `register: aGroupOrCommand`. 


We showed that the message `/` navigates the tree and access the corresponding subtree.
The Spec extension supports also the notion of order and substitution as follows: 
- `registerFirst: aGroupOrCommand`
- `registerLast: aGroupOrCommand`
- `register: aGroupOrCommand after: another` and `register:aGroupOrCommand before: another`
- `register: aGroupOrCommand insteadOf: another`
- unregister:

### Conclusion

Commander and Spec can take advantage of each other. 
Commands represent actions and Spec can use them as objects to perform actions such as menus and toolbar.
