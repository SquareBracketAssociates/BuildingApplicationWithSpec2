## Reuse and composition at work
@cha_reuse

status: Currently working on it. 
status: spellchecked

A key design goal of Spec is to enable the seamless reuse of user interfaces. 
The reason for this is that it results in a significant productivity boost when creating user interfaces.

This focus on reuse was actually already visible in the previous chapters, where we have seen that basic widgets can be used as if they were a complete user interface. In this section we focus on the reuse and composition of presenters, showing that it basically comes for free.
The only requirement when building a UI is to consider how the user interface should be parameterized when it is being reused.

Said differently, in this chapter, you will learn how we can build a new UI by reusing already defined elements.


### First requirements


![ProtocolCodeBrowser: Browsing the public APIs of widgets.](figures/ProtocolBrowser.png width=80&anchor=figprotocolbrowser&label=figprotocolbrowser)

To show how Spec enables the composition and reuse of user interfaces, in this chapter we build the user interface shown in Figure *@figprotocolbrowser@* as a composition of four parts:
1. The **WidgetClassListPresenter**: a widget containing a `SpListPresenter` specifically for displaying the subclasses of `SpAbstractWidgetPresenter`.
1. The **ProtocolMethodListPresenter**: a widget composed of a `SpListPresenter` and a `SpLabelPresenter` for displaying methods of a protocol.
1. The **ProtocolPresenter**:  a composition of one `WidgetClassListPresenter` and two `ProtocolMethodListPresenter`, it will browse the methods of all subclasses of `SpAbstractWidgetPresenter`.
1. The **ProtocolCodeBrowser**: reuses a `ProtocolPresenter`, changes its layout, and adds a `SpTextPresenter` to see the source code of the methods.






### Creating a basic UI to be reused as a widget
@sec_WidgetClassList

The first custom UI we build should display a list of all subclasses of
 the class `AbstractWidgetPresenter`.
This UI will later be reused as a widget for a more complete UI. 
The code is as follows (we do not include code for accessors):

First, we create a subclass of `SpPresenter` with one instance variable `list` which will hold an instance of `SpListModel`.

```
SpPresenter << #WidgetClassListPresenter
    slots: { #list };
    tag: 'MiniClassBrowser';
    package: 'CodeOfSpecBook'
```

In the method `initializePresenters`, we create the list and populate it with the required classes, in alphabetical order.

```
WidgetClassListPresenter >> initializePresenters
    list := self newList.
    list items: (AbstractWidgetModel allSubclasses
                     sorted: [:a :b | a name < b name ]).
    self focusOrder add: list.
```

We also add a title for the window.

```
WidgetClassListPresenter >> initializeWindow: aWindowPresenter
    aWindowPresenter title: 'Widgets'
```

The layout contains only the list:

```
WidgetClassListPresenter >> defaultLayout
    ^ SpBoxLayout newLeftToRight 
        add: #list;
        yourself
```

Doing `WidgetClassListPresenter new open`, you should obtain UI shown in Figure *@fig_WidgetClassList@*.

![WidgetClassListPresenter open.](figures/WidgetClassList.png width=50&label=fig_WidgetClassList)


### Supporting reuse


Since this UI will later be used together with other widgets to provide a more complete user interface, some actions will need to occur when a list item is clicked. However, we cannot know beforehand what all these possible actions will be everywhere that it will be reused. The best solution therefore is to place this responsibility on the reuser of the widget. Every time this UI is reused as a widget, it will be configured by the reuser. To allow this, we add a configuration method named `whenSelectionChangedDo:` as follows:

```
WidgetClassListPresenter >> whenSelectionChangedDo: aBlock
    list whenSelectionChangedDo: aBlock
```


Now, whoever reuses this widget can parameterize it with a block that will be executed whenever the selection changes.

### Combining two basic presenters into a reusable UI
@sec_protocollist

The UI we build now will show a list of all methods of a given protocol, and it combines two widgets: a list and a label. 
Considering reuse, there is no difference from the previous UI. 
This is because the reuse of a UI as a widget is **not impacted at all** by the number of widgets it contains (nor by their position). 
Large and complex UIs are reused in the same way as simple widgets.

```
SpPresenter << #ProtocolMethodListPresenter
    slots: { #label . #methods };
    tag: 'MiniClassBrowser';
    package: 'CodeOfSpecBook'
```


The `initializePresenters` method for this UI is quite straightforward. 
We specify the default label text as 'protocol', which will be changed when the widget is reused. 
We also give this UI a title.

```
ProtocolMethodListPresenter >> initializePresenters
    methods := self newList.
    methods displayBlock: [ :m | m selector ].
    label :=  self newLabel.
    label label: 'Protocol'.
    self focusOrder add: methods.
```


Now to make sure that we can have a nice title when the widget is opened in a window, we
define the method `initializeWindow:`.

```
ProtocolMethodListPresenter >> initializeWindow: aWindowPresenter
    aWindowPresenter title: 'Protocol widget'
```


The layout code builds a column with the fixed-height label on top and the list taking all the space that remains. 
%(See Chapter *@cha_layout_construction@* for more on layouts.)

```
ProtocolMethodListPresenter >> defaultLayout
    ^ SpBoxLayout newTopToBottom 
            add: #label ;
            add: #methods ;
            yourself
```


This UI can be seen by executing `ProtocolMethodList new open`. 
As shown in Figure *@figprotocollist@* the list is empty and the result not really nice. 
This is normal since we did not set any items but we should also place better the elements.

![ProtocolMethodListPresenter with unclear layout.](figures/ProtocolList.png width=50&label=figprotocollist)

```
ProtocolMethodListPresenter >> defaultLayout
    ^ SpBoxLayout newVertical
            add: #label  withConstraints: [:c | c expand: false];
            add: #methods withConstraints: [:c | c fill];
            yourself
```


Now you should get a better look as shown in Figure *@figprotocollist2@*.


![ProtocolMethodListPresenter with nicer layout.](figures/ProtocolList2.png width=50&label=figprotocollist2)

Our protocol method list will need to be configured when it is used, for example, to fill the list of methods and to specify what the name is of the protocol. To allow this, we add a number of configuration methods:

```
ProtocolMethodListPresenter >> items: aCollection
    methods items: aCollection
```

```
ProtocolMethodListPresenter >> label: aText
    label label: aText
```

```
ProtocolMethodListPresenter >> resetSelection
    methods selection unselectAll
```


```
ProtocolMethodListPresenter >> whenSelectionChangedDo: aBlock
    methods whenSelectionChangedDo: aBlock
```


#### Note. 
An alternative to adding these methods is simply to do nothing: since both the methods and the label are accessible through their accessors, a reuser of this widget may simply obtain them and configure them directly. These two alternatives reflect design decisions that we will discuss later in Section *@sec_public_API@*.

### Inspecting live the widgets

Now we can check manually if the widget is working by doing:

```
ProtocolMethodListPresenter new open ; inspect
```


Then in the inspector, we can use the newly created methods to pass a collection of methods:

```
    self items: Point methods
```


![Live coding your widgets (no it is not openWithSpec but open my friend but the background is supercool!).](figures/inspectingLive.png width=90&label=figinspectingLive)

Now we can play and for example, decide to sort the items as follows: 

```
self items: (Point methods sort: #selector ascending)
```


### Writing tests

When we start to feel the need to check manually what we have done, this is a sign that we should write a test instead. 
So let us do that. 
It is easy to write simple tests for widgets when we do not test popups. 
So let us take advantage of that.

We add an accessor to access the method list.

```
ProtocolMethodListPresenter >> methods
    ^ methods
```


```
TestCase << #ProtocolMethodListPresenterTest
    tag: 'MiniClassBrowser';
    package: 'CodeOfSpecBook'
```


```
ProtocolMethodListPresenterTest >> testItems

    | proto methods |
    methods := Point methods sort: #selector ascending.
    proto := ProtocolMethodListPresenter new.
    proto items: methods.
    self assert: proto methods items first class equals: CompiledMethod.
    self assert: proto methods items first selector equals: methods first selector
```


We hope that we convinced you that writing simple UI tests is easy with Spec. 
Do not miss this opportunity to control the complexity of your software. 


### Managing three widgets and their interactions
@sec_protocolviewer

The third user interface we build is a composition of the two previous user interfaces. 
We will see that there is no difference between configuring custom UIs and configuring system widgets: both kinds of widgets are configured by calling methods of the `api` protocol.

This UI is composed of a `WidgetClassListPresenter` and two `ProtocolMethodListPresenter` and specifies that when a model class is selected in the `WidgetClassListPresenter`, the methods in the protocols `api` and `api-events` will be shown in the two `ProtocolMethodListPresenter` widgets.

```
SpPresenter << #ProtocolViewerPresenter
    slots: { #models . #api . #events };
    tag: 'MiniClassBrowser';
    package: 'CodeOfSpecBook'
```


The `initializePresenters` method shows the use of `instantiate:` to instantiate widgets, and some of the different parametrization methods of the `ProtocolMethodListPresenter` class.

```
ProtocolViewerPresenter >> initializePresenters

    models := self instantiate: WidgetClassListPresenter.
    api := self instantiate: ProtocolMethodListPresenter.
    events := self instantiate: ProtocolMethodListPresenter.

    api label: 'api'.
    events label: 'api-events'.

    self focusOrder add: models; add: api; add: events.
```


```
ProtocolViewerPresenter >> initializeWindow: aWindowPresenter
    aWindowPresenter title: 'Protocol viewer'
```


To describe the interactions between the different widgets we define the `connectPresenters` method. It specifies that when a class is selected, the selections in the method lists are reset and both method lists are populated. 
Additionally, when a method is selected in one method list, the selection in the other list is reset.

```
ProtocolViewerPresenter >> connectPresenters

    models whenSelectionChangedDo: [ :selection |
        | class |
        api resetSelection.
        events resetSelection.
        class := selection selectedItem.
        class
            ifNil: [ 
                api items: #(). 
                events items: #() ]
            ifNotNil: [
                api items: (self methodsIn: class for: 'api').
                events items: (self methodsIn: class for: 'api - events') ] ].

    api whenSelectionChangedDo: [ :selection |
         selection selectedItem ifNotNil: [ events resetSelection ] ].
    events whenSelectionChangedDo: [ :selection |
        selection selectedItem ifNotNil: [ api resetSelection ] ]
```


```
ProtocolViewerPresenter >> methodsIn: class for: protocol
    ^ (class methodsInProtocol: protocol) sorted:
             [ :a :b | a selector < b selector ]
```

Lastly, the layout puts the sub-widgets in one column, with all sub-widgets taking the same amount of space.

```
ProtocolViewerPresenter >> defaultLayout
    ^ SpBoxLayout newTopToBottom
        add: #models; 
        add: #api; 
        add: #events;
        yourself
```



As previously, the result can be seen by executing the following snippet of code: `ProtocolViewer new open`, and the result is shown in Figure *@figProtocolViewerVertical@*.

This user interface is functional, clicking on a class will show the methods of the `api` and the `api-events` protocols of that class.

![ProtocolViewer in vertical mode.](figures/ProtocolViewerVertical.png width=50&label=figProtocolViewerVertical)


### Having different layouts


Notice that you can change the layout as follows to get all the widgets in a row as shown in Figure *@figProtocolViewerHorizontal@*.
We will show later that a presenter can have multiple layouts and that the programmer decide which one to use.


We can do better. Let us define three methods as follows: 

```
ProtocolViewerPresenter >> horizontalLayout
    ^ SpBoxLayout newLeftToRight 
        add: #models; 
        add: #api; 
        add: #events;
        yourself
```


```
ProtocolViewerPresenter >> verticalLayout
    ^ SpBoxLayout newTopToBottom 
        add: #models; 
        add: #api; 
        add: #events;
        yourself
```


```
ProtocolViewerPresenter >> defaultLayout
    ^ self verticalLayout
```

Now we can decide to open the viewer with different layouts using the message `openWithSpec:` as follows (as shown in Figure *@figProtocolViewerHorizontal@*):

```
ProtocolViewerPresenter class >> exampleHorizontal
    | inst |
    inst := self new.
    inst openWithLayout: inst horizontalLayout
```


![ProtocolViewer in horizontal mode.](figures/ProtocolViewerHorizontal.png width=50&label=figProtocolViewerHorizontal)


### Enhancing our API


Similar to the second user interface, when this UI is reused it will probably need to be configured. The relevant configuration here is what to do when a selection change happens in any of the three lists. We hence add the following three methods to the `api` protocol.

```
ProtocolViewerPresenter >> whenSelectionInAPIChanged: aBlock
    api whenSelectionChangedDo: aBlock
```

```
ProtocolViewerPresenter >> whenSelectionInClassChanged: aBlock
    models whenSelectionChangedDo: aBlock
```

```
ProtocolViewerPresenter >> whenSelectionInEventChanged: aBlock
    events whenSelectionChangedDo: aBlock
```


**Note.**
These methods add semantic information to the configuration API. They state that they configure what to do when a class, `api` or `api-events` list item has been changed. This arguably communicates the customization API more clearly than just having the sub-widgets accessible.


### Changing the layout of a reused widget

@sec_protocolbrowser

Sometimes, when you want to reuse an existing UI as a widget, the layout of that UI is not appropriate to your needs. Spec allows you to nonetheless reuse such a UI by overriding the layout of its widgets, and we show this here.

Our last user interface reuses the `ProtocolViewerPresenter` with a different layout and adds a text zone to edit the source code of the selected method.

```
SpPresenter << #ProtocolCodeBrowserPresenter
    slots: { #text . #viewer };
    tag: 'MiniClassBrowser';
    package: 'CodeOfSpecBook'
```

```
ProtocolCodeBrowserPresenter >> initializePresenters
    text := self instantiate: SpCodePresenter.
    viewer := self instantiate: ProtocolViewerPresenter.
    text syntaxHighlight: true.
    self focusOrder
        add: viewer;
        add: text
```

```
ProtocolCodeBrowserPresenter >>defaultLayout
    
    ^ SpBoxLayout newTopToBottom
            add: (SpBoxLayout newHorizontal add: #viewer ; yourself);
            add: #text;
            yourself
```

```
ProtocolCodeBrowserPresenter >> initializeWindow: aWindowPresenter
    aWindowPresenter title: 'Spec Protocol Browser'
```

The `connectPresenters` method is used to make the text zone react to a selection in the lists. When a method is selected, the text zone updates its contents to show the source code of the selected method.

```
ProtocolCodeBrowserPresenter >> connectPresenters

    viewer whenSelectionInClassChanged: [ :selection | text behavior: selection selectedItem ].
    viewer whenSelectionInAPIChanged: [ :selection |
        selection selectedItem
            ifNotNil: [ :item | text text: item sourceCode ] ].
    viewer whenSelectionInEventChanged: [ :selection |
        selection selectedItem
            ifNotNil: [ :item | text text: item sourceCode ] ]
```

SHOULD Update 

```
initializePresenters
    
    self instantiate: ProtocolViewer withLayout: ProtocolViewer horizontalLayout
    
```


### Changing layouts

```
presenter := MyPresenter new.
presenter openWithLayout: (SpBoxLayout newTopToBottom
add: #models;
add: #api;
add: #events;
yourself).
```
 
or you can do:

````
presenter := MyPresenter new.
presenter layout: (SpBoxLayout newTopToBottom
add: #models;
add: #api;
add: #events;
yourself).
presenter open.
```

or you can do:

```
presenter := MyPresenter new.
presenter layout: presenter layoutAlternative1.
presenter open.
```

which means you can do your layout choose without requiring to specify a method that will be executed later.


ESTEBAN how I can do

````
ProtocolCodeBrowserPresenter >> initializePresenters
    text := self instantiate: SpCodePresenter.
    viewer := self instantiate: ProtocolViewerPresenter.
    BUT WITH ANOTHER LAYOUT DEFINED ON PROTOCOLVIEWER PRESENTER
    self focusOrder
        add: viewer;
        add: text

```

### Considerations about a public configuration API

@sec_public_API

In this chapter, we have seen several definitions of methods in the public configuration API of the widget being built.
The implementation of our configuration methods here is simply delegated to internal widgets, but a configuration can of course be more complex than that, depending on the internal logic of the UI.

For methods that simply delegate to the internal widgets, the question is whether it makes sense to define these as methods in the `api` protocols at all.
This fundamentally is a design decision to be made by the programmer.
Not having such methods makes the implementation of the widget more lightweight but comes at the cost of a less clear intent and of breaking encapsulation.

For the former cost, we have seen an example in the protocol method list of Section *@sec_protocollist@*.
The presence of the three methods defined there communicates to the user that we care about what to do when a class, `api` or `api-events` list item has been changed.
The same fundamentally also holds for the other examples in this chapter: each method in an `api` protocol communicates an intent to the reuser: this is how we expect that this widget will be configured.
Without such declared methods, it is less clear to the reuser what can to be done to be able to effectively reuse this widget.

For the latter cost, expecting reusers of the widget to directly send messages to internal objects (in instance variables) means breaking encapsulation.
As a consequence, we are no longer free to change the internals of the UI, e.g., by renaming the instance variables to a better name or changing the kind of widget used.
Such changes may break reusers of the widget and hence severely limit how we can evolve this widget in the future.
In the end, it is safer to define a public API and ensure in future versions of the widget that the functionality of this API remains the same.

So in the end it is important to consider future reusers of your UI and the future evolution of your UI.
You need to make a tradeoff of writing extra methods versus possibly making reuse of the UI harder as well as possibly making future evolution of the UI harder.

### New versus old patterns

In Spec 1.0 list presenters were exposing a different API namely `whenSelectedItemChanged:` as in the following example. 

```
initializePresenters

    models := self instantiate: WidgetClassListPresenter.
    api := self instantiate: ProtocolMethodListPresenter.
    events := self instantiate: ProtocolMethodListPresenter.

    api label: 'api'.
    events label: 'api-events’.


connectPresenters

    api whenSelectedItemChanged: [ :method |
        method ifNotNil: [ events resetSelection ] ].
    events whenSelectedItemChanged: [ :method |
        method ifNotNil: [ api resetSelection ] ].
```

In Spec2 list presenters and friends are exposing a different one exposing the selection of the list itself. 
The design rationale is that a selection is a complex object (multi selection...).
So we have 

```
    api whenSelectionChangedDo: [ :selection |
        selection selectedItem ifNotNil: [ events resetSelection ] ].
    events whenSelectionChangedDo: [ :selection |
        selection selectedItem ifNotNil: [ api resetSelection ] ].
```

Now the question for your presenter definers is what is the API that you should expose to your users. 
We would say that it does not matter because exposing a API similar to the one of Spec1 is possible as shown below. 

```
whenSelectedItemChangedDo: aBlock
    methods whenSelectionChangedDo: [ :selection |
        selection selectedItem ifNotNil: [ :i | aBlock value: i ] ]
```


Now we suggest using the Spec20 way because it will give your presenters consistency with the core presenters of Spec
and it will make them easier to collaborate. 


### Conclusion


In this chapter, we have discussed a key point of Spec: the ability to seamlessly reuse existing UIs as widgets. This ability comes with no significant cost to the creator of a UI. The only thing that needs to be taken into account is how a UI can (or should) be customized.

The reuse of complex widgets at no significant cost was a key design goal of Spec because it is an important productivity boost for the writing process of UIs. The boost firstly comes from being able to reuse existing nontrivial widgets, and secondly because it allows you to structure your UI in coherent and more easily manageable sub-parts with clear interfaces. We therefore encourage you to think of your UI as a composition of such sub-parts and construct it modularly, to yield greater productivity.
