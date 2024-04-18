## Using transmissions and ports (Draft)
 
 
Transmissions are a more compact way to connect presenters than events as shown previously. 
 
### What are transmissions? 
 
Transmissions are a way to connect presenters, thinking about the “flow” of information more than the way it is displayed. 
Each presenter defines **output ports** (ports to send information) and **input ports** (ports to receive information). 
There are at least one default input port and one default output port. 
A transmission connects a presenter’s output port with a presenter’s input port. 
 
For example, think on an overview-detail (O->D) relationship, when you navigate the elements in the overview O, you want to see the detail D. This is typically solved by showing a list with list elements and a form with the detail of an element. 
In Spec, this will be declared more or less like this: 
 
``` 
list := self newList.
detail := self newText. 
``` 
 
 
- Input ports define the transmission destination points of a presenter. They handle an incoming transmission and transmit them properly to the target presenter. 
- An output port defines origin actions \(and the possible data associated to such action\) to transmit to a destination \(input\) port. It also defines the transformations to apply to the output data before giving them to the input port. 
 
 
### Transmitting from an output port to an input port 
 
 
A transmission connects a presenter’s output port with a presenter’s input port as shown in the following example: 
``` 
list transmitTo: detail. 
``` 
 
This connects the list presenter default output port with the detail presenter default input port. 
 
### Transforming a transmission 
 
 
The object transmitted from a presenter output port can be inadequate for the input port. To solve this problem a transmission offers the possibility to transform the transmitted object. 
 
This is as simple as using the `transform:` protocol: 
 
SD: is the method called trasmitTo:transform: or juts transmitTo: 

``` 
list 
    transmitTo: detail 
    transform: [ :aValue | aValue asString ]. 
``` 
 
 
 
### Transmitting from an output port to an arbitrary input receiver  

It is possible that the user requires to listen an output port, but instead transmitting the value to another presenter, other operation is needed. 

**todo** christophe I do not get the previous sentence 
 
There is the `transmitDo:` protocol to handle this situation: 
 
``` 
list transmitDo: [ :aValue | aValue crTrace ]. 
``` 
 
 
### Acting after a transmission 
 
 
Sometimes, after a transmission happens, the user needs to react to modify something given the new status achieved by the presenter \(like, pre-selecting something\). 
The `postTransmission:` protocol allows you to handle that situation. 
 
``` 
list 
    transmitTo: detail 
    postTransmission: [ :fromPresenter :toPresenter :value | 
        "something to do here"
        toPresenter enabled: value isEmptyOrNil not ]. 
``` 
 
 
 
 
 
## Transmissions OLD 
 

 
![A simple class browser.](figures/SimpleClassBrowser.png width=80&label=figSimpleClassBrowser) 
 
### Presenter definition 


``` 
SpPresenter << #SpClassMethodBrowserPresenter
    slots: {#methodListPresenter . #textPresenter . #classListPresenter};
    package: 'Spec-Examples-Standalone' 
``` 
 
 
 
### Initializing the presenters 
``` 
SpClassMethodBrowserPresenter >> initializeWidgets
    classListPresenter := self newList.
    methodListPresenter := self newList.
    textPresenter := self newCode.
    
    textPresenter acceptBlock: [ :t | methodListPresenter selectedItem inspect ].
    methodListPresenter displayBlock: #selector.
``` 
 
 
### Now the logic as transmissions 
 
 
In Spec, the wiring between presenters can be expressed at a higher level than the simple low-level dependency and change notification. For this we use transmissions.  
In the following method, we state that when a class list item is selected, it should transmit the list of methods of the selected class sorted.  
Notice that the `transform:` block gets the selectors from the selected class item.  
 
``` 
SpClassMethodBrowserPresenter >> initializePresenter
    classListPresenter 
        transmitTo: methodListPresenter 
        transform: [ :class | class methods sort: #selector descending ] 
        postTransmission: [ :destination :origin :transmited | destination selectIndex: 1 ].

    methodListPresenter
        transmitTo: textPresenter
        transform: [ :method | method ifNil: [ '' ] ifNotNil: [:m | m sourceCode ] ]
        postTransmission: [ :destination :origin :transmited | 
            transmited ifNotNil: [ destination behavior: transmited methodClass ] ] 
``` 
 
 
 Esteban can you explain transmitted ifNotNil: \[ destination behavior: transmitted methodClass \]? 
 
### Layout 
 
 
``` 
SpClassMethodBrowserPresenter class >> defaultSpec
    ^ SpPanedLayout newVertical
        add:
            (SpPanedLayout newHorizontal
                add: #classListPresenter;
                add: #methodListPresenter;
                yourself);
        add: #textPresenter;
        yourself 
``` 
 
 
 
 
Now we define a method to set the class list named `classes:`.  
It will be called to pass the classes that we want to display as in the following: 
 
``` 
| example |
example := SpClassMethodBrowserPresenter new.
example
    classes: self environment allClasses;
    open. 
``` 
 
 
 
``` 
SpClassMethodBrowserPresenter >> classes: aList
    classListPresenter items = aList 
        ifTrue: [ ^ self ].
    classListPresenter
        items: aList;
        selectIndex: 1
```


### Questions for esteban 
- What kind of event can trigger a transmission? 
- I need an example with ports. 
- How can we define ports for a given new component? 
 
``` 
defineOutputPorts 
    ^ { SpListSelectionPort new } 
``` 
 
 
``` 
defineInputPorts 
    ^ { SpRootsPresenterPort new } 
``` 
 
 
What is a ListSelectionPort? 
What is a RootsPresenterPort? 
 
### Conclusion 
