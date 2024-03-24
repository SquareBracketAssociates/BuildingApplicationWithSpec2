## First Contact With Examples

status: spellchecked
status: missing colored list snippet

@cha_first_contact

As a first step in Spec's world, we use a couple of examples to explain its use. We will first construct a small but complete user interface, and then show some more examples of how existing widgets can be configured. This will allow you to build basic user interfaces.

After completing this chapter you should read the following chapter about the reuse of Spec widgets, which is the key behind the power of Spec. With these two chapters, you should be able to construct Spec user interfaces as intended. You could use the rest of this book just as reference material, but nonetheless, we recommend you to at least give a brief look at the other chapters as well.

### A customer satisfaction UI

@sec_customer_satisfaction

![A screen shot of the customer satisfaction UI.](figures/Customers_Basic.png width=50&label=fig_Customers_Basic)

In this first example, we construct a simple customer satisfaction UI, which allows a user to give feedback about a service by clicking on one of three buttons. \(This feedback should be recorded and processed, but this is outside of the scope of this example\). We show a screenshot of the UI in Figure *@fig_Customers_Basic@*.


### Create the class of the UI 


All user interfaces in Spec are subclasses of `SpPresenter`, so the first step in creating the UI is subclassing it:

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
at the class side.

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
All of these are defined in the `widgets` protocol. These are shortcuts to create a presenter for the associated widgets.

The following method shows how `newButton` is defined.

```
SpPresenter >> newButton
	^ self instantiate: SpButtonPresenter
```


Note that the naming may be a bit confusing since we write `newButton` while it
will create a button _presenter_ and not a button _widget_, which Spec will take 
care of by itself. We do not use `newButtonPresenter` to get an API easier to use.

!!note **Do not** call `new` to instantiate a widget that is part of your UI. An alternative way to instantiate widgets is to use the message `instantiate:` with a presenter's class as an argument. For example `screen := self instantiate: SpLabelPresenter.`. This allows one to instantiate standard and non-standard widgets.

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


Third and last, you can optionaly change the focus order of your presenters while using tab, which is useful for keyboard navigation.

```
CustomerSatisfactionPresenter >> initializePresenters
	... continued ...
	"specification of order of focus"
	self focusOrder
		add: buttonHappy;
		add: buttonNeutral;
		add: buttonBad
```


#### Presenter interaction logic


Now we should define what will happen when we press a button. 
We define this in a separate method called `connectPresenters`: 


```
CustomerSatisfactionPresenter >> connectPresenters

	buttonHappy action: [ screen label: buttonHappy label ].
	buttonNeutral action: [ screen label: buttonNeutral label ].
	buttonBad action: [ screen label: buttonBad label ].
```


We use the message `action:` to specify the action that is performed when the buttons are clicked. In this case, we change the content of what is shown on the screen, to provide feedback that the choice has been registered. Note that the message `action:` is part of the button API. In other situations, you will specify that when a given event occurs, another message should be sent to a widget subpart.

!!note To summarize: Specialize `initializePresenters` to define and configure and `connectPresenters` to connect your presenters together.

#### Specifying the widget layout

The widgets have now been defined and configured, but their placement in the UI has not yet been specified. This is the role of the class side method `defaultSpec`.

```
CustomerSatisfactionPresenter class >> defaultSpec
	^ SpBoxLayout newVertical 
		add: (SpBoxLayout newHorizontal
				add: #buttonHappy;
				add: #buttonNeutral;
				add: #buttonBad;
				yourself);
		add: #screen;
		yourself
```


In this layout, we add two rows to the UI, one with the buttons and one with the screen of text. Defining widget layout is a complex process with many different possible requirements, hence in this chapter, we do not talk in detail about layout specification. For more information, we refer to Chapter *@cha_layout_construction@*.

!!note The argument of the `add:` messages are symbols representing the name of the variable containing the presenter to display.



Once the class method `defaultSpec` is defined, you can start to open your UI as follows: `CustomerSatisfactionPresenter new openWithSpec`. You should obtain a widget similar to the one shown in Figure *@figFirstCut@*.

![A first version of the customer satisfaction UI.](figures/FirstCut.png width=50&label=figFirstCut)


### Define a title and window size, open and close the UI


To set the window title and the initial size of your widget, you have to specialize the method `initializeWindow:` as follows: 

```
CustomerSatisfactionPresenter >> initializeWindow: aWindowPresenter
	
	aWindowPresenter
		title: 'Customer Satisfaction Survey';
		initialExtent: 400@100
```


Of course, you are free to use the helper methods to return the title and extent of your widget. 
Now reopening your widget you should get the one displayed in Figure *@figSecondCut@*.


![A final version of the customer satisfaction UI.](figures/SecondCut.png width=50&label=figSecondCut)


To open a UI, an instance of the class needs to be created and it needs to be sent the `openWithSpec` message. This will open a window and return an instance of `SpWindowPresenter`, which allows the window to be closed from the code.

```
	| ui |
	ui := CustomerSatisfactionPresenter new openWithSpec.
	[ ... do a lot of stuff until the UI needs to be closed ...]
	ui close.
```


Note that to update the contents of your window once it is open, you have the method `SpPresenter>>withWindowDo:`.
But we will discuss it later in this book. More information about managing windows: e.g., opening dialog boxes or setting the about text is present in Chapter *@cha_managing_windows@*.

This concludes our first example of a Spec user interface. We now continue with more examples of how to configure the different widgets that can be used in such a user interface.

### Fun with Lists

@sec_listModel

As an illustration of how widgets can be configured, we now show two examples of lists: a list using different background colors and a list that also shows icons.

These examples show two important features of Spec:
1. All widgets can be opened as a window. This is because there is no fundamental difference between a complex UI as shown above and the standard widgets.
1. All widgets can be configured, and their configuration methods are classified in `api` protocols.


### Variations on registered colors

We start with an example of a `SpListPresenter` where the elements represent color names as shown in
(Figure *@figColorNames@*). 
The items held in the list are the names of different colors, and the list shows them simply.
The following code shows how this is done -- note that we sort the names so that you get the same situation than the one in the figure.

```
| registeredColorNamesList |
registeredColorNamesList := SpListPresenter new.
registeredColorNamesList
	items: (Color registeredColorNames sort: #yourself ascending).
(registeredColorNamesList openWithSpec)
	title: 'Registered color names'.
```


![Registered color names.](figures/ColorNames.png width=50&label=figColorNames)

### Manipulating color objects

Now imagine that we have a list of colors and not a list of color names. The items held in the list are different colors.
The following code shows how this is done. Here the string representation of the item is used by default as shown in Figure *@figRegisteredColors@*.

```
| registeredColorsList |
registeredColorsList := SpListPresenter new.
registeredColorsList
	items: (Color allInstances sort: #hue ascending).
(registeredColorsList openWithSpec)
	title: 'Registered colors'.
```


![All colors.](figures/RegisteredColors.png width=50&label=figRegisteredColors)

Now imagine that we should like to have a different representation such as the RGB triplet representation. 
Using the message `displayBlock:`, we configure the list to apply a transformation on the item before displaying it.
The resulting widget is shown in Figure *@figasTriplet@*.

```
| registeredColorsList |
registeredColorsList := SpListPresenter new.
registeredColorsList
	items: (Color allInstances sort: #hue ascending);
	displayBlock: [ :it | it rgbTriplet ].
	(registeredColorsList openWithSpec)
		title: 'Registered colors as triplets'.
```


![All rbg.](figures/asTriplets.png width=50&label=figasTriplet)

### TOBE DONE WHEN AVAILABLE Registered colors  as item background


We start with an example of a `SpListPresenter` where the elements have different background colors, shown in Figure *@fig_modified_background@*. The items held in the list are the names of different colors, and the list shows them using a background of that color.

The following code shows how this is done:

```
| registeredColorsList |
registeredColorsList := SpListPresenter new.
registeredColorsList
	items: Color registeredColorNames;
	displayBlock: [ :item | item ].
(registeredColorsList openWithSpec)
	title: 'Registered colors'.
```


![TO BE DONE List with modified background colors](figures/RegisteredColorsBack.png width=50&label=fig_modified_background)

Here we see the following messages that are part of the `SpListPresenter` API.

- The message `items:` sets the elements of the list.
- The message `displayBlock:` changes the way the element is displayed.



### List of icons


The second example shows a list containing the icons of the current theme and their respective selector in an `SpListPresenter`. The list items are associations of the icon names and the icons themselves, the text that is shown in the list is the icon name, and the icon shown in the list is the icon itself. We also sort the items in the list alphabetically according to their name.

```
| iconList |
iconList := SpListPresenter new.
iconList
	items: Smalltalk ui icons icons associations;
	displayBlock: [ :assoc | assoc key ];
	sortingBlock: [ :assocA :assocB | assocA key < assocB key ];
	icons: [ :elem | elem value ].
(iconList openWithSpec) title: 'Availiable icons for the current theme.'
```


![A list of icons](figures/IconList.png width=50&label=fig_IconList)

The following messages of the `SpListModel` API are noteworthy:
- The message `displayBlock:` takes a block that receives a domain-specific item and should return something that can be displayed in a list, like a String.
- The message `sortingBlock:` takes a block that is used to sort the elements of the list before displaying them.



### Conclusion


In this chapter, we have given you a first contact with Spec user interfaces. We have first shown you what the different steps are to build a user interface with Spec, and then shown you two examples of how to configure existing Spec widgets.

More examples of Spec user interfaces are found in the Pharo image itself. Since all Spec user interfaces are subclasses of `SpPresenter`, they are easy to find and each of them may serve as an example. Furthermore, experimentation with widgets and user interfaces is made easy because all widgets can be opened as standalone windows.

We recommend that you at least read the next chapter about the reuse of Spec widgets, which is the key reason behind the power of Spec. This knowledge will help you in building UIs faster through better reuse, and also allow your own UIs to be reused. The chapter after that on the three pillars of Spec gives a more complete overview of the functioning of Spec and is worthwhile to read in its entirety. Later chapters are intended more as reference material for specific problems or use cases, but can of course be read in full as well.
