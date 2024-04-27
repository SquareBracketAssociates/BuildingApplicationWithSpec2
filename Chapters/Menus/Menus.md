## Menu and menuBar

status: definitively needed

Soon in the best theater...


### Menu

`contextMenu: self todoListContextMenu` sets the context menu to what is defined in the method `todoListContextMenu`. Let us study right now.



```
TodoListPresenter >> initializePresenters
    | addButton |
    todoListPresenter := self newTable
    addColumn: ((SpCheckBoxTableColumn evaluated: [:task | task isDone]) 
            width: 20;
            onActivation: [ :task | task done: true ];
            onDeactivation: [ :task | task done: false ];
            yourself);
    addColumn: (SpStringTableColumn 
            title: 'Title' 
            evaluated: [:task | task title);
    contextMenu: self todoListContextMenu;
    yourself.
```

```
TodoListPresenter >> todoListContextMenu

    ^ self newMenu 
        addItem: [ :item | item 
                        name: 'Edit...'; 
                        action: [ self editSelectedTask ] ];
        addItem: [ :item | item 
                        name: 'Remove'; 
                        action: [ self removeSelectedTask ] ]
```

### Menu Bar

### ToolBar

How to create one that we can resue across components?

Attention un `SpToolBarButton` Family should be contained in a `SpToolBar`


Homemade toolbar with SpButton