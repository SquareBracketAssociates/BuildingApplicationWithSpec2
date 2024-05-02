## Most of Spec in one example
@chacasestudyone

status: ready for review I got an error with ports so should check with esteban
status: grammarly passed

In this chapter, we will guide you through the building of a simple but not trivial application to manage films as shown in Figure *@FullApp@*.
We will show many aspects of Spec that we will revisit in depth in the rest of this book: the application, presenters, the separation between domain and model, layout, transmissions to connect widgets,  and styles.

![Film App: reusing the same component to edit and browsing a film.](figures/FullApp.png width=90&label=FullApp)

### Application

Spec20 introduces the concept of an application. An _application_ is a small object responsible for keeping the state of your application. It manages, for example, the multiple windows that can compose your application, and its backend (Morphic or Gtk), and can hold properties shared by the application.

We start to define an application as follows:

```
SpApplication << #ImdbApp
    package: 'Spec2-TutorialOne'
```

In this example, we will show how we define which backend to use and this will allow us to switch between Morphic and GTK.

### A basic film model

Since we will manage films we define a `Film` class as follows: It has a name, a year, and a director. We generate the companion accessors.

```
Object << #ImdbFilm
    slots: {#name . #year . #director};
    package: 'Spec-TutorialOne'
```


We need to have a way to store and query some films. We could use Voyage [https://github.com/pharo-nosql/voyage](https://github.com/pharo-nosql/voyage) since it works without an external Mongo DB. But we want to keep it extremely simple.
So let's define a kind of singleton.

We define a _class_ instance variable called `films`.

```
Object class << ImdbFilm class
    slots: { #films }
```


We define a method that lazy initializes the `films` variable to an ordered collection.

```
ImdbFilm class >> films

    ^ films ifNil: [ films := OrderedCollection new ]
```

And to finish we define a way to add a film to the list.

```
ImdbFilm class >> addFilm: aFilm

    films add: aFilm
```

Now we are ready to define a first presenter that manages a list of films.

### List of films

We define a presenter to manage a list of films by defining a new class named `ImdbFilmListPresenter` which inherits from `SpPresenter`. We add an instance variable named `filmList` that will hold an elementary list presenter.

Note that a presenter such as `ImdbFilmListPresenter` can have multiple subpresenters.

```
SpPresenter << #ImdbFilmListPresenter
    slots: { #filmList };
    package: 'Spec-TutorialOne'
```


We define how the information should be presented by defining a method named `defaultLayout`. Here we define a simple layout: a list of boxes displayed vertically. We are declaring that inside this box layout, there is a specific presenter for the list itself and it is named `filmList`.

#### defaultLayout

```
ImdbFilmListPresenter >> defaultLayout

    ^ SpBoxLayout newTopToBottom
        add: filmList;
        yourself
```

When you do not define any other methods to represent layout, `defaultLayout` is the method that is invoked by Spec logic.

A presenter can have subpresenters e.g., `ImdbFilmListPresenter` will contain table presenters and you will see later that
1. a presenter can have multiple layouts and
2. that layouts can be defined dynamically.

In Spec, layouts are dynamic by default and are expressed at the instance level. To allow backward compatibility, it is still possible to define a `defaultLayout` _class_ side method that returns a layout instead of using a `defaultLayout` instance side method but it is not the recommended way.

#### initializePresenters

So far, we have not initialized `filmList`. In fact, `filmList` is a variable holding another presenter.

The place to initialize the subpresenters is in the method `initializePresenters` as shown below. Here we define that `filmList` is a table with three columns. The message `newTable` instantiates a `SpTablePresenter`.

In fact, the list is more a table than a mere list: we describe that we want three columns. This is why we use a table and not a simple list. We define it as follows:

```
ImdbFilmListPresenter >> initializePresenters

    filmList := self newTable
        addColumn: (SpStringTableColumn title: 'Name'
            evaluated: #name);
        addColumn: (SpStringTableColumn title: 'Director'
            evaluated: #director);
        addColumn: (SpStringTableColumn title: 'Year'
            evaluated: #year);
        yourself
```



![A layout and a simple `initializePresenters` showing an empty list of films.](figures/FilmList-01-LayoutInitilalizePresenters.png width=60&label=LayoutInitilalizePresenters)


### Filling up the film list

We define the method `updatePresenter` which is automatically invoked after `initializePresenters`. It just queries the domain (`ImdbFilm`) to get the list of the recorded films and populates the internal table with them. Right now we do not have any film in the singleton so the list of films is empty.

```
ImdbFilmListPresenter >> updatePresenter

    filmList items: ImdbFilm films
```

The following expression creates an instance of the film list presenter and opens it. You get the window shown in Figure *@LayoutInitilalizePresenters@*.

```
ImdbFilmListPresenter new open
```

If you want, just add manually a film and reopen the presenter and you should see the film in the list.

```
ImdbFilm addFilm: (ImdbFilm new
    name: 'E.T.';
    director: 'Steven Spielberg';
    year: '1982';
    yourself)
```

### Opening presenters via the application

While directly creating a presenter is possible during development,
a more canonical way to create a presenter is to ask the application using the message `newPresenter:` as follows.

```
| app |
app := ImdbApp new.
(app newPresenter: ImdbFilmListPresenter) open
```

The application is responsible for managing windows and other information, therefore it is important to use it to create presenters that compose the application.

### Improving the window

A presenter can be embedded in another presenter as we will show later. It can be also placed within a window and this is what the message `open` is doing. Spec offers another hook, the method `initializeWindow:` to specialize the information presented when a presenter is displayed within a window.

![Film list presenter with a toolbar and decorated window.](figures/FilmList-02-initalizeWindows.png width=60&label=figFilmListPresenter2)

The method `initializeWindow:` allows you to define a title, a default size (message `initialExtent:`), and a toolbar.

```
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

You should obtain the window with a toolbar as shown in Figure *@figFilmListPresenter2@*.
To make sure that the `Add film` button does not raise an error, we trigger an `addFilm` method
that is defined with no behavior.
In fact, we will define a different presenter to be able to define a film.

```
ImdbFilmListPresenter >> addFilm

    "empty for now"
```

As we will see in Chapter *@cha_commander@*, toolbars can be automatically created out of commands.
We could have added the toolbar in a similar way to the `filmList` (e.g. using an instance variable) as part of the `ImdbFilmListPresenter` because the toolbar is also a presenter (similar to the table presenter or other predefined presenters). But doing it that way is less modular.
Note also that the toolbar we created could be factored in a separate class to increase reuse too.



### An application manages icons

What we can see from the definition of the previous method `initializeWindow:` is that an application manages also icons via the message `iconNamed:`. Indeed, a presenter defines the `iconNamed:` message as a delegation to its application.
In addition your application can define its own icon set using the message `iconManager:`.

### FilmPresenter itself


We are ready to define a simple presenter to edit a film. We will use it to add a new film or simply display it.
We create a new subclass of `SpPresenter` named `ImdbFilmPresenter`. This class has three instance variables: `nameText`, `directorText`, and `yearNumber`.

```
SpPresenter << #ImdbFilmPresenter
    slots: { #nameText . #directorText . #yearNumber};
    package: 'Spec-TutorialOne'
```


As we did previously, we define a default layout by defining the method `defaultLayout`.
This time we use a grid layout. With a simple grid layout, you can define the position in the grid where your presenters will appear.

```
ImdbFilmPresenter >> defaultLayout

    ^ SpGridLayout new
        add: 'Name' at: 1@1; add: nameText at: 2@1;
        add: 'Director' at: 1@2; add: directorText at: 2@2;
        add: 'Year' at: 1@3; add: yearNumber at: 2@3;
        yourself
```


Note that it is not required to create the accessors for the presenter elements as we were forced to do it in Spec 1.0.
Here we only create getters because we will need them when creating the corresponding `ImbdFilm` instance.

```
ImdbFilmPresenter >> year

    ^ yearNumber text

ImdbFilmPresenter >> director

    ^ directorText text

ImdbFilmPresenter >> name

    ^ nameText text
```

For convenience, a `SpGridLayout` also comes with a builder that lets you add elements to the layout in the order they will appear. The previous layout definition can be rewritten as:

```
ImdbFilmPresenter >> defaultLayout

    ^ SpGridLayout build: [ :builder |
        builder
            add: 'Name'; add: nameText; nextRow;
            add: 'Director'; add: directorText; nextRow;
            add: 'Year'; add: yearNumber ]
```

Pay attention, do not add a `yourself` message here. Because you would return the class and not the layout instance.

![A single film presenter.](figures/FilmList-03-OpenFilmPresenter.png width=50&label=figFilmPresenter1)

And similarly, as before, we define the method `initializePresenters` to initialize the variables to the corresponding elementary presenters. Here `nameText` and `directorText` are initialized to a text input, and `yearNumber` is a number input.

```
ImdbFilmPresenter >> initializePresenters

    nameText := self newTextInput.
    directorText := self newTextInput.
    yearNumber := self newNumberInput
                rangeMinimum: 1900 maximum: Year current year;
                yourself.
```


Now we can try our little application with the following script and obtain a situation similar to the one shown in Figure *@figFilmPresenter1@*:

```
| app |
app := ImdbApp new.
(app newPresenter: ImdbFilmPresenter) open.
```



### Better looking FilmPresenter

We improve the look of the film presenter by specifying column behavior and setting window properties.
As you can see, the form to present a Film data has very large labels. Indeed, they take half of the form width.
We can solve that by using non-homogenous columns and asking the second column to take the biggest possible width with `column:expand:`. See Figure *@FilmListPresenter2@*

```
ImdbFilmPresenter >> defaultLayout

    ^ SpGridLayout build: [ :builder |
        builder
            beColumnNotHomogeneous;
            column: 2 expand: true;
            add: 'Name'; add: nameText; nextRow;
            add: 'Director'; add: directorText; nextRow;
            add: 'Year'; add: yearNumber ]
```

![Using the non homogenous grid layout.](figures/FilmList-04-OpenFilmPresenter-2.png width=50&label=FilmListPresenter2)

We now set the window properties by adding the following new `initializeWindow:` method. See Figure *@FilmListPresenter3@*.

```
ImdbFilmPresenter >> initializeWindow: aWindowPresenter

    aWindowPresenter
        title: 'Film';
        initialExtent: 400 @ 250
```

![Better window.](figures/FilmPresenter3.png width=50&label=FilmListPresenter3)

### Customizing the modal Dialog

Spec lets us adapt the dialog window, for example, to add interaction buttons.
Here we specialize the method `initializeDialogWindow:` to add two buttons that control the behavior of the application (as shown in Figure *@Customizeddialog@*).

```
ImdbFilmPresenter >> initializeDialogWindow: aDialogPresenter

    aDialogPresenter centered.
    aDialogPresenter
        addButton: 'Cancel' do: [ :presenter | presenter close ];
        addButton: 'Save Film' do: [ :presenter | presenter beOk; close ].
```

![Customizing the dialog window.](figures/FilmList-05-Modal.png width=60&label=Customizeddialog)


### Invoking a presenter

We are ready to use the film presenter from the film list presenter. We can define the method `addFilm` in the class `ImdbFilmListPresenter`. When the user clicks on the button, we create a new film presenter that we associate with the current application.

We open such a presenter as a modal dialog using the message `openModal`. When the user presses the "Save Film" button (that we have customized in Figure *@Customizeddialog@*), we add a new film
to our little database and we update the list as shown in Figure *@refreshed@*.

```
ImdbFilmListPresenter >> addFilm

    | presenter windowPresenter |
    presenter := ImdbFilmPresenter newApplication: self application.
    windowPresenter := presenter openModal.
    windowPresenter isOk
        ifFalse: [ ^ self ].
    ImdbFilm
        addFilm:
            (ImdbFilm new
                name: presenter name;
                director: presenter director;
                year: presenter year).
    self updatePresenter
```


We can now open the `FilmListPresenter` and click on the `Add film` button. Once the film data entered, and the `Save Film` button clicked, you will see that the FilmListPresenter is updated with the film we just added.

```
app := ImdbApp new.
(app newPresenter: ImdbFilmListPresenter) open.
```

![Film list gets refreshed.](figures/FilmList-06-ListRefreshed.png width=60&label=refreshed)


### Embedding a FilmPresenter into the FilmList

We have two main visual elements:  one for a list of films and one for film details. We can imagine that we would like to see the film details in the same container as the list, especially because a film description is larger than the list columns.

Let us proceed. First, we add a new instance variable named `detail` to the class `ImdbFilmListPresenter`.

```
SpPresenter << #ImdbFilmListPresenter
    slots: { #filmList . #detail };
    package: 'Spec-TutorialOne'
```

We redefine the default layout. We will show later that we can have different layouts.

```
ImdbFilmListPresenter >> defaultLayout

    ^ SpBoxLayout newTopToBottom
        add: filmList;
        add: detail;
        yourself
```


Finally, since we are going to use this presenter in different places, we will need to add a method to control whether it is editable or not:

```
ImdbFilmPresenter >> editable: aBoolean

    nameText editable: aBoolean.
    directorText editable: aBoolean.
    yearNumber editable: aBoolean
```

Now we improve the `initializePresenters` of `ImdbFilmListPresenter`.
- First we instantiate `ImdbFilmPresenter`.
- Second, we configure it as read-only by sending the `editable: false` message.
- Third we define that, when an element of the list is selected, we should display the information in the detail presenter. While we can express this in the `initializePresenters` method, we prefer to specify it in the `connectPresenters` method. See Section *@section_define_component_communication@*.


```
ImdbFilmListPresenter >> initializePresenters

    filmList := self newTable
        addColumn: (SpStringTableColumn title: 'Name'
             evaluated: #name);
        addColumn: (SpStringTableColumn title: 'Director'
              evaluated: #director);
        addColumn: (SpStringTableColumn title: 'Year'
             evaluated: #year);
        yourself.

    detail := self instantiate: ImdbFilmPresenter.
    detail editable: false
```

### Define component communication
@section_define_component_communication

We add a little helper method named `setModel:` in class `ImdbFilmPresenter` to be able to pass a film
and populate the presenter accordingly. We will use this method in the following method.

```
ImdbFilmPresenter >> setModel: aFilm

    nameText text: aFilm name.
    directorText text: aFilm director.
    yearNumber number: aFilm year.
```

Note that the method `setModel:` is needed only if you do not subclass from `SpPresenterWithModel`. If you subclass from `SpPresenter`, it is the only way to have the model initialized before the setup of the presenter (and avoid errors when opening the presenter).

Defining interactions between presenters is done in the `connectPresenters` method. We implement it to define that, when an element of the list is selected, we should display the information in the detail presenter. It is worth taking some time to look at the `whenSelectionChangedDo:` message.

The `whenSelectionChangedDo:` method expects a block with zero or one argument. Such an argument is not the selected item directly but a more complex object that represents the selection. Indeed a selection is different in a single selection list and a multiple selection list. Therefore Spec defines the notion of selection mode under the form of subclasses of `SpAbstractSelectionMode`.

```
ImdbFilmListPresenter >> connectPresenters

    filmList whenSelectionChangedDo: [ :selectedItemMode |
        selectedItemMode isEmpty
             ifFalse: [ detail setModel: selectedItemMode selectedItem ] ].
```

With `connectPresenters` in place, selecting an item in the list results in showing the details of the selected item, as shown in Figure *@embedded@*.

![Embedding the film description in the list: selecting a list item populates the detailed visual component.](figures/FilmList-07-embedded.png width=60&label=embedded)


### Testing your application UI

A strong aspect of Spec is the fact that we can write tests about the interaction and the logic of your UI. Tests are so powerful to help us create nice designs and make sure that we can spot our errors, that we give you the chance to see that writing tests for a UI is not complex.

We define a subclass of `TestCase`.

```
TestCase << #FilmListPresenterTest
    package: 'Spec-TutorialOne'
```


```
FilmListPresenterTest >> testWhenSelectingOneFilmThenDetailIsUpdated

    | presenter detail |
    "Prepare the context"
    presenter := ImdbFilmListPresenter new.
    presenter open.
    detail := presenter detail.
    self assert: detail name isEmpty.

    "Act"
    presenter clickFilmAtIndex: 1.

    "Assert"
    self deny: detail name isEmpty.
    presenter delete
```


As you see, we will need to define two methods on `ImdbFilmListPresenter` to support proper testing: a getter for `detail` and an interaction method `clickFilmAtIndex:`.
We categorize them in a `testing - support` protocol to show they are only useful for testing purposes.

```
ImdbFilmListPresenter >> clickFilmAtIndex: anIndex

    filmList clickAtIndex: anIndex

ImdbFilmListPresenter >> detail

    ^ detail
```

This test is a bit poor because we do not test explicitly the value of the name of the film in the detailled component. We did this to keep the test set up simple, partly because `ImdbFilm` stores the current films globally (Singletons are ugly and they also make testing more complex).

We define three helper methods on ImdbFilm to reset the stored films and add the E.T. film.

```
ImdbFilm class >> reset

    films := OrderedCollection new
```


```
ImdbFilm class >> addET

    films add: self ET
```


```
ImdbFilm class >> ET

    ^ self new
        name: 'E.T.';
        director: 'Steven Spielberg';
        year: '1982';
        yourself
```


Now we can define a method `setUp`.

```
FilmListPresenterTest >> setUp

    super setUp.
    ImdbFilm reset.
    ImdbFilm addET
```

Now we update the test to keep the opened presenter in an instance variable. This allows us to define a `tearDown` method that always closes the presenter, no matter if the test succeeds or fails.

```
FilmListPresenterTest >> testWhenSelectingOneFilmThenDetailIsUpdated

    | detail |
    "Prepare"
    presenter := ImdbFilmListPresenter new.
    presenter open.
    detail := presenter detail.
    self assert: detail name isEmpty.

    "Act"
    presenter clickFilmAtIndex: 1.

    "Assert"
    self deny: detail name isEmpty
```


```
FilmListPresenterTest >> tearDown

    presenter delete.
    super tearDown
```

### Adding more tests

Tests are addictive because we can change programs and check that they still work and limit our stress.
So we will write another one.

Let us add the following getter method to support our tests.

```
ImdbFilmListPresenter >> filmList

    ^ filmList
```

Let us test that a list has one film and that if we select a non-existent index, the name is still the last valid selected one.

```
FilmListPresenterTest >> testWhenSelectingOneFilmAndClickingOnEmpty

    | name |
    "Prepare"
    presenter := ImdbFilmListPresenter new.
    presenter open.

    "Act"
    presenter clickFilmAtIndex: 1.

    "Assert"
    name := presenter detail name.
    self deny: name isEmpty.
    self assert: presenter filmList listSize equals: 1.

    presenter clickFilmAtIndex: 2.
    self assert: presenter detail name equals: name
```


Since we do not really understand what would be to set the list as multiple selection we test it.

```
FilmListPresenterTest >> testListIsSimpleSelection

    presenter := ImdbFilmListPresenter new.
    presenter open.
    self deny: presenter filmList isMultipleSelection
```


What you see is that it relatively simple to test that the interaction you specified is actually working as expected.


### Changing layout

With Spec, a presenter can have multiple layouts and even layouts that are created on the fly as we will see with dynamic layouts. We can decide which layout to use to open a presenter. Let us illustrate it. Imagine that we prefer to have the list above the film details or just the list alone.

```
ImdbFilmListPresenter >> listAboveLayout

    ^ SpBoxLayout newTopToBottom
        add: detail;
        add: filmList;
        yourself
```

The following example shows that we can open `ImdbFilmListPresenter` with the layout `listAboveLayout` that we just defined.

```
| app presenter |
app := ImdbApp new.
presenter := app newPresenter: ImdbFilmListPresenter.
presenter openWithLayout: presenter listAboveLayout.
```

We can also define a layout with a part of the subpresenters.
Here `listOnlyLayout` only shows the list.

```
ImdbFilmListPresenter >> listOnlyLayout

    ^ SpBoxLayout newTopToBottom
        add: filmList;
        yourself
```

The following example shows that we can open `ImdbFilmListPresenter` with one layout and dynamically change it by another layout.
In the playground do not declare the temporary variables so that they are bound and kept in the playground.

```
app := ImdbApp new.
presenter := app newPresenter: ImdbFilmListPresenter.
presenter open.
```

The presenter is opened with the default layout, now in the playground execute the following line.

```
presenter layout: presenter listOnlyLayout.
```

Now you can see that the layout with only one list has been applied dynamically.

![A presenter can have multiple layouts for its subpresenters.](figures/FilmList-08-embeddedAbove.png width=60&label=changedlayout)


### Using transmissions

Spec 2.0 introduces a nice optional concept to propagate selection from one presenter to another, thinking about the "flow" of information more than the implementation details of this propagation, which can change from presenter to presenter.

With transmissions, each presenter can define a set of output ports (ports to transmit information) and input ports (ports to receive information). Widget presenters already have defined the output/input ports you can use with them, but you can add your own ports to your presenters.

The easiest way to declare a transmission is by sending the `transmitTo:` message from one presenter to another. We can now update the `connectPresenters` method to use transmissions.

```
ImdbFilmListPresenter >> connectPresenters

    filmList transmitTo: detail.
```

Here, `filmList` is a table that will transmit its selection to `detail` presenter.

Let us explain a bit. `ImdbFilmPresenter` is a custom presenter. Spec does not know how to "fill" it with input data. We need to tell Spec that `ImdbFilmPresenter` model will be the input port and receive the input data. Therefore we need to define an input port as follows:


```
ImdbFilmPresenter >> inputModelPort

    ^ SpModelPort newPresenter: self
```

```
ImdbFilmPresenter >> defaultInputPort

    ^ self inputModelPort
```


Note that we could have inlined `inputModelPort` definition into the `defaultInputPort` definition.

The input data will be set by using the `setModel:` method we already defined on `ImdbFilmPresenter`.

You can now open the application and see that it still behaves as expected.

```
| app |
app := ImdbApp new.
(app newPresenter: ImdbFilmListPresenter) open.
```

""Esteban"" I do not understand why this is not working. I get an error with the ports.
""Enzo"" I think the error comes from the fact `setModel:` is called when the presenter is initialized and so there is no selection, thus a nil parameter. I managed to deal with it by adding a nil guard clause in `setModel:` but I wonder if we could make a "default selection" that would be the first film so that the presenter starts with a film to display details.

### Styling the application

Different elements in an application can have different look and feels, for example to change the size or color of a font for a header. To support this, Spec introduces the concept of "styles" for components.

In Spec, an application defines a Stylesheet (or a set of them). This defines a set of "style classes" that can be later assigned to presenter widgets. Defining a style class, however, works differently for each backend. While Gtk accepts (mostly) regular CSS to style your widgets, Morphic has its own sub-framework.

An application comes with a default configuration and a default stylesheet. If you do not need to style your application, there is no need to define them. In our example, we would like to define a `header` style to customize some labels. In Spec every presenter understands the message `addStyle:` that adds a tag (a CSS class) to the receiver.


To do so, you need to declare a stylesheet in a configuration. The configuration itself needs to be declared in your application. We will define a new presenter for the label and tag it with a specific CCS class using the message `addStyle:`. Our CCS class will be named `'customLabel'`.

First, we create the specific configuration for our application.

```
SpMorphicConfiguration << #ImdbConfiguration
    package: 'Spec-TutorialOne'
```


```
ImdbApp >> initialize

    super initialize.
    self
        useBackend: #Morphic
        with: ImdbConfiguration new.
```

We can now define our custom styles. The easiest way is to create a style from a String. Here we define that an element using the tag `customLabel` will be drawn in red.

```
ImdbConfiguration >> customStyleSheet

    ^ '
.application [
    .customLabel [ Font { #color: #red } ] ]'
```

Pay attention not to forget the '.' in front of `application` and `customLabel`
We specialize the method `configure:` so that it includes the custom style as follows:

```
ImdbConfiguration >> configure: anApplication

	super configure: anApplication.
	self addStyleSheetFromString: self customStyleSheet.
```

We are ready to use the tag for the label.
Up until now, Spec was creating automatically a presenter for the label but it was not accessible from the developer. 
Therefore we have to add a label explicitly so that we can tag it with a 'CSS' like class. This is what the message `addStyle: 'customLabel'` below is doing.

We add a `nameLabel` instance variable to `ImdbFilmPresenter` to get a label and we initialize it in the method `initializePresenters` as follows:

```
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



Then we update the layout to use the newly defined label presenter.

```
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

![Styled film description](figures/FilmList-styling.png width=60&label=FilmListPresenterStyled)

### Conclusion

We saw that with Spec the developer defines how a visual element (a presenter) is composed of other visual elements.
Such a presenter has the responsibility to describe the interaction with other presenters but also with the domain objects. It has also the responsibility to describe its visual aspect.
