## A 10 min small example
@chaSmallExample

We will construct a small but complete user interface, and then show some more examples of how existing widgets can be configured. This will allow you to build basic user interfaces.

After completing this chapter you should read Chapter *@cha_reuse@* about reuse of Spec widgets, which is the key behind the power of Spec. With these two chapters, you should be able to construct Spec user interfaces as intended. You could use the rest of this book just as reference material, but nonetheless we recommend you to at least give a brief look at the other chapters as well.

### A customer satisfaction UI
@seccustomersatisfaction

![A screen shot of the customer satisfaction UI.](figures/CustomersBasic.png width=50&label=figCustomersBasic)

We construct a simple customer satisfaction UI, which allows a user to give feedback about a service by clicking on one of three buttons. \(This feedback should be recorded and processed, but this is outside of the scope of this example\). We show a screenshot of the UI in Figure *@figCustomersBasic@*.


### Create the class of the UI 


All user interfaces in Spec are subclasses of `SpPresenter`, so
the first step in creating the UI is subclassing it:

```
SpPresenter << #CustomerSatisfactionPresenter
    slots: { #buttonHappy . #buttonNeutral . #buttonBad . #screen};
    package: 'CodeOfSpec20Book'
```



The instance variables of the class hold the _presenters_ the UI contains.
In this case, we have three buttons and a text screen.

The methods of the class provide the initialization and configuration of the presenters, e.g.,
labels and actions, as well as the logic of their interaction. The
basic design of our GUI, i.e., how the widgets are laid out, is defined by a method
at class side.

### Instantiate and configure subwidgets


A subclass of `SpPresenter` has the responsibility to define the
`initializePresenters` method, which instantiates and configures the
widgets used in the user interface. We will now discuss it piece by piece.
Note that since this method may be a bit long we will split it into pieces that represent
their intent.

#### Presenter creation

```
CustomerSatisfactionPresenter >> initializePresenters
    screen := self newLabel.
    buttonHappy := self newButton.
    buttonNeutral := self newButton.
    buttonBad := self newButton.
```


`SpPresenter` defines messages for the creation of standard presenters: `newButton`, `newCheckBox`, `newDropList`, ...
All of these are defined in the `widgets` protocol. These are shortcuts to create presenter for the associated widgets.

The following method shows how `newButton` is defined.

```
SpPresenter >> newButton
    ^ self instantiate: SpButtonPresenter
```


Note that the naming may be a bit confusing since we write `newButton` while it
will create a button _presenter_ and not a button _widget_, which Spec will take 
care by itself. We do not use `newButtonPresenter` to make the API easier to use.

!!note **Do not** call `new` to instantiate a widget that is part of your UI. An alternative way to instantiate widgets is to use the message `instantiate:` with a presenter's class as argument. For example `screen := self instantiate: SpLabelPresenter`. This allows one to instantiate standard and non-standard widgets.

#### Presenter configuration

Second, we configure the buttons of our UI. The message `label:` defines their label and the message `icon:` specifies the icon that will be displayed near the label.

```
CustomerSatisfactionPresenter >> initializePresenters
    ... continued ...
    screen label: 'Please give us your feedback.'.
    buttonHappy
        label: 'Happy';
        icon: (self iconNamed: #thumbsUp).
    buttonNeutral
        label: 'Neutral';
        icon: (self iconNamed: #user).
    buttonBad
        label: 'Bad';
        icon: (self iconNamed: #thumbsDown).
```

You can browse the default icons available in Pharo by executing the following line:

```
Smalltalk ui icons inspect.
```

Feel free to add your own icons as well.


#### Presenter interaction logic

Now we should define what will happen when we press a button. 
We define this in a separate method called `connectPresenters`: 


```
CustomerSatisfactionPresenter >> connectPresenters
    buttonHappy action: [ screen label: buttonHappy label ].
    buttonNeutral action: [ screen label: buttonNeutral label ].
    buttonBad action: [ screen label: buttonBad label ].
```


We use the message `action:` to specify the action that is performed when the button is clicked. In this case, we change the content of what is shown on the screen, to inform the user that the choice has been registered. Note that the message `action:` is part of the button API. In other situations, you will specify that when a given event occurs, another message should be sent to a widget subpart.

!!note To summarize:

- specialize `initializePresenters` to define and configure the presenters that are the elements of your UI;
- speciallize `connectPresenters` to connect those presenters together and specify their interaction.

#### Specifying the widget layout

The widgets have now been defined and configured, but their placement in the UI has not yet been specified. This is the role of the method `defaultLayout`.

```
CustomerSatisfactionPresenter >> defaultLayout
    ^ SpBoxLayout newVertical 
        add: (SpBoxLayout newLeftToRight
                add: buttonHappy;
                add: buttonNeutral;
                add: buttonBad;
                yourself);
        add: #screen;
        yourself
```


In this layout, we add two rows to the UI, one with the buttons and one with the screen of text. Defining widget layout is a complex process with many different possible requirements, hence in this chapter we do not talk in detail about layout specification. For more information we refer to Chapter *@cha_layout_construction@*.


Once the class method `defaultLayout` is defined, you can start to open your UI as follows: `CustomerSatisfactionPresenter new open`. You should obtain a widget similar to the one shown in Figure *@figFirstCut@*.

![A first version of the customer satisfaction UI.](figures/FirstCut.png width=50&label=figFirstCut)


### Define a title and window size, open and close the UI


To set the window title and the initial size of your widget, you have to specialize the method `initializeWindow:` as follows: 

```
CustomerSatisfactionPresenter >> initializeWindow: aWindowPresenter    
    aWindowPresenter
        title: 'Customer Satisfaction Survey';
        initialExtent: 400@100
```


You are free to use helper method to return the title and extent of your widget. Now if you reopen your widget, you should get the one displayed in Fig. *@figSecondCut@*.


![A final version of the customer satisfaction UI.](figures/SecondCut.png width=50&label=figSecondCut)


To open a UI, an instance of the class needs to be created and it needs to be sent the `open` message. This will open a window and return an instance of `SpWindowPresenter`, which allows the window to be closed from code.

```
| ui |
ui := CustomerSatisfactionPresenter new open.
[ ... do a lot of stuff until the UI needs to be closed ...]
ui close.
```


Note that to update the contents of your window once it is open, you have the method `SpPresenter>>withWindowDo:`.
But we will discuss it later in this book. More information about managing windows: e.g., opening dialog boxes or setting the about text is present in Chapter *@cha_managing_windows@*.

This concludes our first example of a Spec user interface. We now continue with more examples on how to configure the different widgets that can be used in such a user interface.

### Conclusion


In this chapter we have given you a little example of Spec user interfaces. We have first shown you what the different steps are to build a user interface with Spec.

More examples of Spec user interfaces are found in the Pharo Image itself. Since all Spec user interfaces are subclasses of `SpPresenter`, they are easy to find and each of them may serve as an example. Furthermore, experimentation with widgets and user interfaces is made easy because all widgets can be opened as standalone windows.

We recommend that you at least read Chapter *@cha_reuse@* about reuse of Spec widgets, which is the key reason behind the power of Spec. This knowledge will help you in building UIs faster through better reuse, and also allow your own UIs to be reused. 
