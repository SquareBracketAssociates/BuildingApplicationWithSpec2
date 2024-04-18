
 
 
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
