## Most of Spec20 in one example
@chacasestudyone


In this chapter we will guide you over the building of a simple but not trivial 
application to manage films as shown in Figure *@FullApp@*.
We will show many aspects of Spec20 that we will revisit in depth in the rest of this book:
application, presenter, separation between domain and model, layout, transmission to connect widgets, and styles. 

![Film App: reusing the same component to edit and browsing a film.](figures/FullApp.png width=90&label=FullApp)

### Application

Spec20 introduces the concept of an application. An _application_ is a small object responsible to keep the state of your application. 
It manages, for example, the multiple windows that can compose your application, its back-end (Morphic or Gtk), and can hold properties shared by the application.

We start to define an application as follows: 

```language=Smalltalk
SpApplication << #ImdbApp
	package: 'Spec2-TutorialOne'
```

In this example, we will show how we define which back-end to use and this will allow us to switch between Morphic and GTK.

### A basic film model

Since we will manage films we define a `Film` class as follows: It has a name, a year and a director. We generate the companion accessors.

```language=Smalltalk
Object << #ImdbFilm
	slots: {#name . #year . #director};
	package: 'Spec2-TutorialOne'
```


We need to have a way to store and query some films. 
We could use Voyage [https://github.com/pharo-nosql/voyage](https://github.com/pharo-nosql/voyage) since it works without an external Mongo DB. 
But we want to keep it extremely simple.
So let's define a kind of singleton (one of the most misunderstood Design Pattern).

We define a _class_ instance variable called `films` (you can use the expand menu to get the full definition on class side).

```language=Smalltalk
Object class << ImdbFilm class
	slots: { #films }
```


We define a method that lazy initializes the `films` variable to an ordered collection.

```language=Smalltalk
ImdbFilm class >> films
	^ films ifNil: [ films := OrderedCollection new ]
```


And to finish we define a way to add a film to the list.

```language=Smalltalk
ImdbFilm class >> addFilm: aFilm
	films add: aFilm
```

Now we are ready to define a first presenter that will manage a list of films.

### List of films

We define a presenter to manage a list of films by defining a new class named `ImdbFilmListPresenter` which inherits from `SpPresenter`.
We add an instance variable named `filmList` that will hold an elementary list presenter. 

Note that a presenter such that `ImdbFilmListPresenter` can have multiple subpresenters. 

```language=Smalltalk
SpPresenter << #ImdbFilmListPresenter
	slots: { #filmList };
	package: 'Spec2-TutorialOne'
```


We define how the information should be presented by defining a method named `defaultLayout`. 
Here we define a simple layout: a list of boxes displayed vertically.
We are declaring that inside this box layout there is a specific presenter for the list itself and it is named `filmList`.

#### defaultLayout

```language=Smalltalk
ImdbFilmListPresenter >> defaultLayout

	^ SpBoxLayout newTopToBottom
		add: filmList; 
		yourself
```

When you do not define any other methods to represent layout, `defaultLayout` is the method that is invoked by Spec logic.

A presenter can have subpresenters e.g., `ImdbFilmListPresenter` will contain table presenters and you will see later that 
- (1) a presenter can have multiple layouts, 
- (2) that we can get more dynamic situations.

In Spec20, layouts are by default dynamic and are expressed at the instance level. To allow backward compatibility, it is still possible to define a `defaultLayout` _class_ side method that returns a layout instead of using a `defaultLayout` instance side method but it is not the recommanded way.

#### initializePresenters

So far, we did not initialized `filmList`. 
In fact, `filmList` is a variable pointing to another presenter.

The place to initialize the subpresenters is in the method `initializePresenters` as shown below. Here we define that `filmList` is a table with three columns. The message `newTable` instantiate a `SpTablePresenter`.

In fact the list is more a table than a mere list: we describe that we want three columns. This is why we use a table and not a simple list. We define it as follows:

```language=Smalltalk
ImdbFilmListPresenter >> initializePresenters

	filmList := self newTable
		addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
		addColumn: (SpStringTableColumn title: 'Director' evaluated: #director);
		addColumn: (SpStringTableColumn title: 'Year' evaluated: #year);
		yourself.
```

At this point `ImdbFilmListPresenter new open` opens an empty list as shown in *@LayoutInitilalizePresenters@*.

### Filling up the film list

We define the method `updatePresenter` which is automally invoked after `initializePresenters`. 
It just queries the domain (`ImdbFilm`) to get the list of the recorded films and populates the internal table with them.

```language=Smalltalk
ImdbFilmListPresenter >> updatePresenter
	
	filmList items: ImdbFilm films
```

The following expression creates an instance of the film list presenter and open it. You get the widget shown in Figure *@LayoutInitilalizePresenters@*.

```language=Smalltalk
ImdbFilmListPresenter new open
```


![A layout and initializePresenters.](figures/FilmList-01-LayoutInitilalizePresenters.png width=60&label=LayoutInitilalizePresenters)


### Opening presenters 

While directly creating a presenter is possible during development,
a more canonical way to create a presenter is to ask the application using the message `newPresenter:` as follows.

```language=Smalltalk
| app |
app := ImdbApp new. 
(app newPresenter: ImdbFilmListPresenter) open.
```

The application is responsible for managing windows and other information, therefore it is important to use it to create presenters that compose the application. 

### Improving window

A presenter can be embedded in another presenter as we will show later. 
It can be also placed within a window and this is what the message `open` is doing. 
Spec offers another hook, the method `initializeWindow:` to specialize the information presented when a presenter is displayed within a window. 

The method `initializeWindow:` allows you to define a title, a default size (message `initialExtent`) and a toolbar. 
 

```language=Smalltalk
ImdbFilmListPresenter >> initializeWindow: aWindowPresenter

	aWindowPresenter 
		title: 'Mini IMDB';
		initialExtent: 600@400;
		toolbar: (self newToolbar
					add: (self newToolbarButton 
							label: 'Add film' ;
							icon: (self iconNamed: #smallAdd);
							action: [ self addFilm ];
							yourself);
						yourself)
```


You should obtain the window with a toolbar as shown in Figure *@FilmListPresenter2@*.
To make sure that the `Add film` button does not raise an error, we trigger an `addFilm` method
that is defined with no behaviour.
In fact we will define a different presenter to be able to define a film. 

```language=Smalltalk
ImdbFilmListPresenter >> addFilm
	"empty for now"
```


As we will see with the Commander2 chapter, toolbars can be automatically created out of commands.

We could have added the toolbar in a similar way than the `filmList` (e.g. using an instance variable) as part of the `ImdbFilmListPresenter` because the toolbar is also a presenter (similar to the table presenter or other predefined presenters). But doing this way is less modular.
Note also that the toolbar we created could be factored in a separate class to increase reuse too.

![Film list presenter.](figures/FilmList-02-initalizeWindows.png width=60&label=FilmListPresenter2)


### An application manages icons

What we can see from the definition of the method `initializeWindow:` is that an application manages also icons via the message `iconNamed:`. Indeed, a presenter defines the `iconNamed:` message as a delegation to its application.
It means that your application can use its own icon set using the method `iconManager:`. 

### FilmPresenter itself


We are ready to define a simple presenter to edit a film. We will use it to add a new film or simply display it.
We create a new subclass of `SpPresenter` named `ImdbFilmPresenter`.
This class has three instance variables: `nameText`, `directorText`, and `yearNumber`.

```language=Smalltalk
SpPresenter << #ImdbFilmPresenter
	slots: { #nameText . #directorText . #yearNumber};
	package: 'Spec2-TutorialOne'
```


As we did previously, we define a default layout by defining the method `defaultLayout`.
This time we use a grid layout. With a simple grid layout you can define the position in the grid where your elements will appear. 

```language=Smalltalk
ImdbFilmPresenter >> defaultLayout 
	^ SpGridLayout new 
		add: 'Name' at: 1@1; add: nameText at: 2@1;
		add: 'Director' at: 1@2; add: directorText at: 2@2;
		add: 'Year' at: 1@3; add: yearNumber at: 2@3;
		yourself
```


Note that it is not required to create the accessors for the presenter elements as we were forced to do it in Spec1.0.
Here we only create getters because we will need them when creating the corresponding `ImbdFilm` instance.

```language=Smalltalk
ImdbFilmPresenter >> yearNumber
	^ yearNumber text

ImdbFilmPresenter >> director
	^ directorText text

ImdbFilmPresenter >> name
	^ nameText text
```

For convenience, a GridLayout also comes with a builder that let you add elements to the layout in the order they will appear. The previous layout definition can be rewritten as:

```language=Smalltalk
ImdbFilmPresenter >> defaultLayout 
	^ SpGridLayout build: [ :builder |
		builder 
			add: 'Name'; add: nameText; nextRow;
			add: 'Director'; add: directorText; nextRow;
			add: 'Year'; add: yearNumber ]
```

Pay attention, do not add a `yourself` message here. Because you would return the class and not the layout instance.

And similarly as before, we define the method `initializePresenters` to initialize the variables to the corresponding elementary presenters. 
Here the `nameText` and `directorText` are initialized to a textInput, and `yearNumber` is a numberInput.

```language=Smalltalk
ImdbFilmPresenter >> initializePresenters

	nameText := self newTextInput.
	directorText := self newTextInput.
	yearNumber := self newNumberInput 
				rangeMinimum: 1900 maximum: Year current year;
				yourself.
```


Now we can try our little application with the following script: 
```language=Smalltalk
| app |
app := ImdbApp new. 
(app newPresenter: ImdbFilmPresenter) open.
```



![Single Film presenter.](figures/FilmList-03-OpenFilmPresenter.png width=60&label=FilmPresenter1)


### Better looking FilmPresenter

We improve the look of the the film presenter by specifying columns behaviour and setting window properties.

As you can see, the form to present a Film data has very large labels. Indeed, they take half of the form width.
We can solve that by using non homogenous columns and ask the second column to take the biggest possible width (`#beExpand`).

```language=Smalltalk
ImdbFilmPresenter >> defaultLayout
	^ SpGridLayout build: [ :builder |
		builder
			beColumnNotHomogeneous;
			column:2 withConstraints: #beExpand;
			add: 'Name'; add: nameText; nextRow;
			add: 'Director'; add: directorText; nextRow;
			add: 'Year'; add: yearNumber ]
```


![Using the non homogenous grid layout.](figures/FilmList-04-OpenFilmPresenter-2.png width=60&label=FilmListPresenter2)


We now set the window properties by adding the following new `initializeWindow:` method (See Figure *@FilmListPresenter3@*).

```language=Smalltalk
ImdbFilmPresenter >> initializeWindow: aWindowPresenter
	aWindowPresenter
		title: 'Film';
		initialExtent: 400 @ 250
```


![Better window.](figures/FilmPresenter3.png width=60&label=FilmListPresenter3)

### Customizing the modal Dialog

Spec lets us adapt the dialog window, for example to add interaction buttons.
Here we specialize the method `initializeDialogWindow:` to add two buttons that control the behavior of the application (as shown in Figure *@Customizeddialog@*).

```language=Smalltalk
ImdbFilmPresenter >> initializeDialogWindow: aDialogPresenter

	aDialogPresenter centered.
	aDialogPresenter 
		addButton: 'Cancel' do: [ :presenter | presenter close ];
		addButton: 'Save Film' do: [ :presenter | presenter beOk; close ].
```

![Customizing the dialog window.](figures/FilmList-05-Modal.png width=100&label=Customizeddialog)


### Invoking a presenter

We can define the method `addFilm` in the class `ImdbFilmListPresenter`. 
When the user clicks on the button, we create a new film presenter that we associate with the current application. 

We open such presenter as a modal dialog. 
When the user press the Ok button (that we have customized in Figure *@FilmListPresenter3@*), we add a new film
to our little database and we update the list as shown in Figure *@refreshed@*.

```language=Smalltalk
ImdbFilmListPresenter >> addFilm
	| dialog windowPresenter |
	dialog := ImdbFilmPresenter newApplication: self application.
	windowPresenter := dialog openModal.
	windowPresenter isOk
		ifFalse: [ ^ self ].
	ImdbFilm
		addFilm:
			(ImdbFilm new
				name: dialog name;
				director: dialog director;
				year: dialog yearNumber).
	self updatePresenter
```


We can now open the `FilmListPresenter` and click on the `Add film` button. Once the film data entered, and the `Save Film` button clicked,
you will see that the FilmListPresenter is updated with the film we just added.

```language=Smalltalk
app := ImdbApp new. 
(app newPresenter: ImdbFilmListPresenter) open.
```

![Film list gets refreshed.](figures/FilmList-06-ListRefreshed.png width=60&label=refreshed)


### Embedding a FilmPresenter into the FilmList

We have two main visual elements:  one for a list of films and one for a film details.
We can imagine that we would like to see the film details just in the same
container than the list, especially since a film description should be larger than the list columns. 

Let us proceed. First we add a new instance variable named `detail` to the class `ImdbFilmListPresenter`.

```language=Smalltalk
SpPresenter << #ImdbFilmListPresenter
	slots: { #filmList . #detail};
	package: 'Spec2-TutorialOne'
```

We redefine the default layout. We will show later that we can have different layouts.

```language=Smalltalk
ImdbFilmListPresenter >> defaultLayout 
	^ SpBoxLayout newTopToBottom
		add: filmList; 
		add: detail;
		yourself
```


We add a little helper method in class `ImdbFilmPresenter` to be able to pass a film
and populate the presenter accordingly. 

```language=Smalltalk
ImdbFilmPresenter >> setModel: aFilm

	nameText text: aFilm name.
	directorText text: aFilm director.
	yearNumber number: aFilm year.
```


Note that we need to define the method `setModel:` is needed only if you do not subclass from `SpPresenterWithModel`. If you subclass from `SpPresenter`, it is the only way to have the model initialized before the setup of the presenter (and avoid errors when opening the presenter).

Now we improve the `initializePresenters` of `ImdbFilmListPresenter`.
- First we instantiate `ImdbFilmPresenter`.
- Second we configure it as readonly using the `enabled: false` message. 

""Note CD: Esteban???"" is it the best way to do that? it is not intuitive + does not work in current Spec2. maybe we shoudl add a #beReadOnly method on presenter calling #enabled: true. Also the method comment says "enable means clickable or focusable"
- Third we define that, when an element of the list is selected, we should display the information in the detail presenter. 


```language=Smalltalk
ImdbFilmListPresenter >> initializePresenters

	filmList := self newTable
		addColumn: (SpStringTableColumn title: 'Name' evaluated: #name);
		addColumn: (SpStringTableColumn title: 'Director' evaluated: #director);
		addColumn: (SpStringTableColumn title: 'Year' evaluated: #year);
		yourself.
		
	detail := self instantiate: ImdbFilmPresenter.
	detail enabled: false.
	
	filmList whenSelectionChangedDo: [ :selectedItemMode | 
		selectedItemMode isEmpty ifFalse: [detail setModel: selectedItemMode selectedItem] ].
```

Definining interactions between presenters is done in the `connectPresenters` method. We will define it to define that, when an element of the list is selected, we should display the information in the detail presenter.
It is worth to take some time to look at `whenSelectionChangedDo:` message.

The `whenSelectionChangedDo:` method expects a block with zero or one argument. Such argument is not the selected item directly but a more complex object representing the selection. Indeed a selection is different in single item selection list and a multiple selection list. Therefore Spec defines the notion of selection mode under the form of subclasses of `SpAbstractSelectionMode`.

```language=Smalltalk
ImdbFilmListPresenter >> connectPresenters

	filmList whenSelectionChangedDo: [ :selectedItemMode | 
		selectedItemMode isEmpty ifFalse: [ detail setModel: selectedItemMode selectedItem ] ].
```


![Embedding the film description in the list: selecting a list item populates the detailed visual component.](figures/FilmList-07-embedded.png width=60&label=embedded)


### Testing your application UI

A strong aspect of Spec is the fact that we can write tests about the interaction and the logic of your UI.
And tests are so powerful to help us create nice design and make sure that we can spot our errors that we give you the chance to see that writing tests for UI is not complex. 

We define a subclass of `TestCase`.

```
TestCase << #FilmListPresenterTest
	package: 'Spec2-TutorialOne'
```


```
FilmListPresenterTest >> testWhenSelectingOneFilmThenDetailIsUpdated

	| list detail |
	"Arrange"
	list := ImdbFilmListPresenter new.
	list open.
	detail := list detail.
	self assert: detail name isEmpty.

	"Act"
	list clickFilmAtIndex: 1.

	"Assert"
	self deny: detail name isEmpty.
	list delete.
```


As you see, we will need to define two methods on `ImdbFilmListPresenter` to support proper testing: a getter for `detail` and an interaction method `clickFilmAtIndex:`.
We will categorize them in a `testing - support` category to show they are only useful for testing purposes.

```
ImdbFilmListPresenter >> clickFilmAtIndex: anIndex 
	filmList clickAtIndex: anIndex

ImdbFilmListPresenter >> detail
	^ detail
```


This test is a bit poor because we do not test explicit the value of the name of the film in the detailled component. 
We did this to keep the test set up simple, partly because `ImdbFilm` stores globally the current films (Remember we told you that singleton are ugly and they also make testing more complex).

We define three helper methods on ImbdFilm to reset the stored films and add E.T. film.

```
ImbdFilm class >> reset 
	films := OrderedCollection new
```


```
ImbdFilm class >> addET
	films add: self ET
```


```
ImbdFilm class >> ET
	^ self new 
		name: 'E.T.';
		director: 'Steven Spielberg';
		year: '1982'; 
		yourself
```


So let us fix this:

```
FilmListPresenterTest >> setUp
	super setUp. 
	ImdbFilm reset. 
	ImdbFilm addET.
```

We will also update the test to keep the opened presenter in an instance variable, allowing us to define a `tearDown` method that will always close the presenter, no matter if the test succeeds or fails.

```
FilmListPresenterTest >> testWhenSelectingOneFilmThenDetailIsUpdated

	| detail |
	"Arrange"
	presenter := ImdbFilmListPresenter new.
	presenter open.
	detail := presenter detail.
	self assert: detail name isEmpty.

	"Act"
	presenter clickFilmAtIndex: 1.

	"Assert"
	self deny: detail name isEmpty.
```



```
FilmListPresenterTest >> tearDown
	presenter ifNotNil: [ presenter delete ].
	super tearDown. 
```






### Adding more tests

Tests are so much fun and addictive (they are because we can change programs and check that they still works and limit our stress), that we will write another one.

Let us add the following method to support our tests.

```
ImdbFilmListPresenter >> filmList
	^ filmList
```

Let us test that a list has one film and that if we select a not existing index, the name is still the last valid selected one.

```
FilmListPresenterTest >> testWhenSelectingOneFilmAndClickingOnEmpty

	| name |
	"Arrange"
	presenter := ImdbFilmListPresenter new.
	presenter open.

	"Act"
	presenter clickFilmAtIndex: 1.

	"Assert"
	name := presenter detail name.
	self deny:  name isEmpty. 
	self assert: presenter filmList listSize equals: 1.

	presenter clickFilmAtIndex: 2.
	self assert: presenter detail name equals: name
```


Since we do not really understand what would be to set the list as multiple selection we test it. 

```
FilmListPresenterTest >> testListIsSimpleSelection

	presenter := ImdbFilmListPresenter new.	
	presenter open.
	self deny: presenter filmList isMultipleSelection.
```


What you see is that it relatively simple to test that the interaction you specified is actually working as expected. 


### Changing layout 

With Spec, a presenter can have multiple layouts and even layouts that are created on the fly as we will see with the dynamic layouts.
We can decide which layout to use to open a presenter.

Let us illustrate it, imagine that we prefer to have the list above the film details, or just the list alone.

```language=Smalltalk
ImdbFilmListPresenter >> listAboveLayout

	^ SpBoxLayout newTopToBottom
		add: detail;
		add: filmList; 
		yourself
```


The following example shows that we can open `ImdbFilmListPresenter` with the layout `listAboveLayout` that we just defined.

```language=Smalltalk
| app presenter |
app := ImdbApp new. 
presenter := app newPresenter: ImdbFilmListPresenter.
presenter openWithLayout: presenter listAboveLayout.
```


We can also only layout part of the subpresenters.
Here `listOnlyLayout` only shows the list.

```language=Smalltalk
ImdbFilmListPresenter >> listOnlyLayout 

	^ SpBoxLayout newTopToBottom
		add: filmList; 
		yourself
```


The following example shows that we can open `ImdbFilmListPresenter` with one layout and dynamically change it by another layout.
In the playground do not declare the temporaries variables so that they are bound and kept in the playground.

```language=Smalltalk
app := ImdbApp new. 
presenter := app newPresenter: ImdbFilmListPresenter.
presenter open.
```

The presenter is opened with the default layout, now in the playground execute the following line. 

```language=Smalltalk
presenter layout: presenter listOnlyLayout.
```

You can now see that the list only layout is applied dynamically.

![A presenter can have multiple layouts for its subpresenters.](figures/FilmList-08-embeddedAbove.png width=60&label=embedded)

	
### Using transmissions

Spec20 introduces a nice optional concept to propagate selection from one presenter to another, thinking on the "flow" of information more than the implementation details of this propagation, which can change from presenter to presenter.

With transmissions, each presenter can define a set of output ports (ports to transmit information) and input ports (ports to receive information). Widget presenters already have defined the output/input ports you can use with them, but you can add your own ports to your presenters.

The easiest way to declare a transmission is by sending the `transmitTo:` message from a presenter to another. We can now update the `connectPresenters` method to use transmissions.

```language=Smalltalk
ImdbFilmListPresenter >> connectPresenters
	"Transmiting from my filmList to a detail"
	
	filmList transmitTo: detail.
```


Here, `filmList` is a table which will transmit its selection to `detail` presenter.
It is an instance of `ImdbFilmPresenter` that we defined, hence it does not define any input port.
Therefore we need to define an input port as follows: 
	
```language=Smalltalk
ImdbFilmPresenter >> defaultInputPort

	^ self inputModelPort
```


```language=Smalltalk
ImdbFilmPresenter >> inputModelPort

	^ SpModelPort newPresenter: self
```

The input data will be set by using the `setModel:` method we already defined on `ImdbFilmPresenter`.

You can now open the application and see that it still behaves like expected.
```language=Smalltalk
| app |
app := ImdbApp new. 
(app newPresenter: ImdbFilmListPresenter) open.
```
Let us explain a bit. `ImdbFilmPresenter` is a custom presenter. Spec does not know how to "fill" it with input data. We need to tell Spec that `ImdbFilmPresenter` model will be the input port and receive the input data. We could inline `inputModelPort` into `defaultInputPort`.


Use the same component to show a detail or edit a film.
""note CD SD:"" What would you like to show here?

### Styling the application

Different elements in an application can have different look and feels, for example to change the size or color of a font for a header. To allow this, Spec introduces the concept of "styles" for components. 

In Spec, one application defines a Stylesheet (or a set of them). 
This defines a set of "style classes" that can be later assigned to presenter widgets. 
Defining a style class, however, work very differently for each backend. 
While Gtk will accept (mostly) regular CSS to style your widgets, Morphic has its own sub-framework. 

An application comes with a default configuration and a default style sheet. If you do not need to style your application, there is no need to define them.
In our example we would like to define a `header` style to customize some labels. To do so, you need to declare a stylesheet in a configuration. The configuration itself needs to be declared in your application.

```language=Smalltalk
ImdbApp >> initialize

 	super initialize.
	self
		useBackend: #Morphic 
		with: ImdbConfiguration new.
```


We create the specific configuration for our application.
```
SpMorphicConfiguration << #ImdbConfiguration
	package: 'Spec2-TutorialOne'
```

We can now define our custom styles. The easiest way is to create a style from a String and then use the appropriate reader to convert it into a style sheet object.
```
ImdbConfiguration >> customStyleSheet
	^ (SpStyleVariableSTONReader fromString: '.application [
		customLabel [ Draw { #color: #red } ] ]
	')
```

```
ImdbConfiguration >> newStyleSheet

	^ SpStyle defaultStyleSheet copy, self customStyleSheet
```

```language=Smalltalk
ImdbFilmPresenter >> initializePresenters 
	nameLabel := self newLabel 
		label: 'Name'; 
		addStyle: 'customLabel'; 
		yourself.
	nameText := self newTextInput.
	directorText := self newTextInput.
	yearNumber := self newNumberInput 
			rangeMinimum: 1900 maximum: Year current year;
			yourself.
```

We add a `nameLabel` instance variable to `ImdbFilmPresenter`. 
We then update the layout to use the newly defined label presenter.

```language=Smalltalk
ImdbFilmPresenter >> defaultLayout
	
	^ SpGridLayout build: [ :builder |
		builder
			beColumnNotHomogeneous;
			column:2 withConstraints: #beExpand;
			add: nameLabel; add: nameText; nextRow;
			add: 'Director'; add: directorText; nextRow;
			add: 'Year'; add: yearNumber ]
```

We can now see that the name label of a film detail has been styled.

![Styled film description](figures/FilmList-styling.png width=100&label=FilmListPresenterStyled)

### Conclusion

We saw that with Spec the developer define how a visual element \(a presenter\) is composed out other visual elements. 
Such presenter has the responsibility to describe the interaction with other presenters but also with the domain objects. It has also the responsibility to describe its visual aspect.