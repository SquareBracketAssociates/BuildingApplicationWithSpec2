## Widgets





### Notebook by example

We will study the iceberg repositories browser


```
initializePresenters

	repositoryNotebook := self newNotebook
```


The `updatePresenter` method is the place where pages are dynamically added to the notebook.
We see that a Panel is added inside the page.

```
updatePresenter

	self model repositoryGroups do: [ :group |
		repositoryNotebook 
			addPageTitle: group label 
			provider: [ 
				(self instantiate: IceTipRepositoryGroupPanel on: group)
					transmitDo: [ "self refreshCommands" ];
				yourself ] ].
	self refresh
```