# Actions in Spec — what they are and how they work

Every interactive application has *things the user can do*: delete a method, open a test runner, toggle a panel, submit a form. In a classic widget toolkit those things are implemented ad hoc: a closure passed to a button, another closure passed to a menu item, a third one bound to a key combination. The same intention is written three times, with three chances to get enablement, labels and shortcuts out of sync.

Spec models that intention as a first-class object: an **action**. An action is defined once and can then be rendered as a menu item, a toolbar button, a toggle button, or a keyboard shortcut — with one place to decide whether it is enabled and whether it is visible.

This article explains:

1. what an action is and how to create one,
2. how actions are grouped and attached to presenters,
3. how enablement, visibility, state and context work,
4. how the same action tree becomes menus, toolbars and shortcuts,
5. how the Pulsar IDE layers its own convention on top of the same machinery.

The focus is on *using* actions. We will not implement a Spec backend.

---

## 1. What an action is

The class comment of `SpAction` summarizes it precisely:

> An action is a spec command (`SpCommand`) that will be used for context menus and/or shortcuts in the presenters that implement `SpTActionContainer`.

Two ideas are packed there:

- **It is a command.** An action inherits from `SpCommand`, which is a Spec-facing command decorator built on Commander2. The command supplies the behavior (`execute`), the enablement hook (`canBeExecuted`) and the context in which it runs.
- **It is a UI-facing decoration.** The action also carries what a user interface needs: a label, a description, an icon, a shortcut key, a display strategy (hide or disable when it cannot run), and a position strategy (left or right in a toolbar).

You do not need the whole class hierarchy to use actions, but this table is a useful map:

| Layer | What it gives you |
| --- | --- |
| `SpAction` | A block-based action: name, icon, shortcut, enablement, visibility, state. |
| `SpCommand` | The Spec-facing command decorator: icon provider and presenter-building hooks. |
| `CmUICommand` | The Commander2 UI decoration: icon, shortcut, display and position strategies. |
| `CmCommand` | The underlying command definition: execution and context. |

`SpCommand` is a decorator: it wraps a command and delegates the basics to it. `SpAction` is a UI-facing command from the start; it does not need to be converted from a domain command before it can be shown.

Because the action *is* the command, the same object is what a menu item fires, what a toolbar button triggers, and what a key binding invokes. There is no separate "menu action" and "button action" — only one action rendered in several places.

If you remember the old `SpShortcut` / `addTool:` API, it is gone. The migration notes are in [§9](#9-migration-and-disambiguation).

---

## 2. Creating actions

### 2.1 Factories

The common constructors are class-side factories on `SpAction`:

```smalltalk
SpAction newName: 'Add' action: [ self doAdd ].
SpAction newName: 'Add' iconName: #add action: [ self doAdd ].
SpAction newName: 'Add' shortcutKey: $a ctrl action: [ self doAdd ].
SpAction newShortcutKey: $a ctrl action: [ self doAdd ].
```

`newShortcutKey:action:` is the "keyboard only" case. It creates an action flagged with `beShortcutOnly`, which simply sets `visible := false`. It will never appear in a menu or toolbar, but it will still be installed as a shortcut.

For anything beyond name/icon/shortcut, the cascade style is more convenient — and it is what most Spec code uses:

```smalltalk
SpAction new
    priority: 10;
    name: 'Add';
    description: 'Add a new element';
    iconName: #add;
    action: [ self doAdd ];
    yourself.
```

`yourself` is the usual Smalltalk idiom for ending a cascade by answering the receiver instead of the last message's result.

The attributes you will use most:

| Message | Meaning |
| --- | --- |
| `name:` / `name` | Label shown in menus and tooltips. |
| `description:` | Longer explanation, used as tooltip/help. |
| `iconName:` | Icon looked up through the action's icon provider. |
| `shortcutKey:` | A key combination (`$a ctrl`, `Character escape asKeyCombination`, …). |
| `priority:` | Ordering inside a group. A fresh action has priority `0`; the getter falls back to `1` only if the slot is `nil`. |
| `action:` | The block executed when the action fires. |
| `actionEnabled:` | Enablement hook: a block, optionally taking the context. |
| `actionVisible:` | Visibility hook: a block, optionally taking the context. |
| `beShortcutOnly` | Hide from menus and toolbars; only install the shortcut. |
| `state:` / `actionState:` | Make the action stateful. This does **not** make it a toggle by itself. |
| `beToggleButton` | Render the action as a toolbar toggle button. |

A minimal example you can run in a workspace:

```smalltalk
| app text |
app := SpApplication new.
text := SpTextPresenter newApplication: app.
text actionsWith: [ :root |
    root addActionWith: [ :action |
        action
            name: 'Beep';
            shortcutKey: $b ctrl;
            action: [ 'Beep' crTrace ] ] ].
text open.
```

The rest of this article explains each piece of that example.

### 2.2 What happens when an action fires

Execution is the core behavior, and `SpAction` implements it in a few lines:

```smalltalk
execute
    action ifNil: [ ^ self ].
    self canBeExecuted ifFalse: [ ^ self ].

    action
        cull: self context
        cull: self
```

Three things are worth noticing:

1. **The action block is optional.** An action without a block does nothing.
2. **The action re-checks enablement before running.** The UI may be slightly stale, or the action may be invoked programmatically; the guard is always there.
3. **The block is invoked with `cull:`, twice.** `cull:` evaluates a block with as many arguments as it can accept. The same `action:` block may therefore be written as:
   - `[ ... ]` — no arguments,
   - `[ :context | ... ]` — receives the action's context,
   - `[ :context :action | ... ]` — receives the context and the action itself.

   A one-argument block receives *only* the context; `cull:` does not pass the action to it.

Smalltalk methods without an explicit `^` answer `self`. So `execute` answers the action, and the block's return value is discarded. That keeps `execute` chainable without changing the action's protocol.

The *context* is the object the action operates on. It is not set automatically for plain Spec actions; see [§5.3](#53-where-the-context-comes-from).

### 2.3 Enablement

`canBeExecuted` is defined on the command layer and refined by `SpAction`:

```smalltalk
canBeExecuted
    actionEnabled ifNil: [ ^ true ].
    ^ actionEnabled cull: self context
```

So `actionEnabled:` must be a block: a plain boolean would receive `cull:` and raise a `MessageNotUnderstood`. Because the block is called with `cull:`, it may take zero or one argument:

```smalltalk
SpAction new
    action: [ self openTestRunner ];
    actionEnabled: [ self hasSelectedProject ]
```

```smalltalk
SpAction new
    actionEnabled: [ :aContext | aContext canRun ];
    action: [ :aContext | aContext run ]
```

The second form only works when the action has a context. In plain Spec, set it explicitly or use a zero-argument block that closes over `self`; in Pulsar, the action collector sets it. This is the most common source of surprises when moving code between Spec and Pulsar.

The UI queries `canBeExecuted` when it builds or refreshes the surface that shows the action. Menus are rebuilt when opened; toolbars may need the presenter to refresh.

### 2.4 Visibility

Visibility is a separate concern from enablement:

```smalltalk
isVisible
    ^ visible and: [
        actionVisible isNil or: [ actionVisible cull: self context ] ]
```

`actionVisible:` is useful for feature flags and context-dependent menus. Pulsar, for instance, hides LLM actions when the corresponding feature is disabled:

```smalltalk
each actionVisible: [ :aContext |
    aContext application settings isFeatureEnabled: #llm ]
```

`beShortcutOnly` is a special case: it sets `visible := false`, so the action never appears in a menu or toolbar. It can still be installed as a shortcut.

### 2.5 State and toggles

Actions can carry state. There are two related ideas:

- `state:` sets a boolean state.
- `actionState:` computes the state from the context.

Both make the action **stateful** (`hasState` answers true), but neither selects a toggle presenter. To render a toolbar toggle, use `beToggleButton`:

```smalltalk
SpAction new
    name: 'Show line numbers';
    beToggleButton;
    state: self isShowLineNumbers;
    action: [ :aContext :theAction |
        self showLineNumbers: theAction state ];
    yourself.
```

`beToggleButton` flips `state` before `execute` is called. If your block needs the new value, take the action as the second argument. The first argument is still the context; it is unused in the example above, which is fine.

For state that is derived from the application, use `actionState:`:

```smalltalk
group addActionWith: [ :action |
    action
        name: 'Repositories';
        description: 'Toggle git repositories';
        actionState: [ :aContext | aContext hasPresenterClass: PulsarRepositoriesView ];
        action: [ :aContext | aContext doOpenRepositories: action state ] ]
```

Here the action is stateful but uses the default toolbar button; it is not a visual toggle. If you want a visual toggle, use `beToggleButton` with a boolean `state:` instead of `actionState:`.

---

## 3. Action groups

Actions are almost never declared one by one. They form a tree of **action groups**: a root group, optionally containing subgroups, each containing actions or other groups. Groups give the UI structure (submenus, toolbar menus) and give application code a way to address a subset of actions by id.

`SpActionGroup` is the Spec-facing group. `SpBaseActionGroup` exists only to provide a root for groups that do not have a parent command group; its class comment says users should not use it directly.

### 3.1 Declaring groups

The idiomatic way to build a group is with the class-side constructor that evaluates a block in the new group:

```smalltalk
SpActionGroup newName: 'Editing' with: [ :root |
    root
        addGroup: #file with: [ :group |
            group
                name: 'File';
                beDisplayedAsSubMenu;
                addActionWith: [ :action |
                    action
                        name: 'Save';
                        iconName: #save;
                        shortcutKey: $s ctrl;
                        action: [ self save ] ] ];
        addActionWith: [ :action |
            action
                name: 'Undo';
                shortcutKey: $z ctrl;
                action: [ self undo ] ] ].
```

Inside the block you use the group DSL:

```smalltalk
addActionWith: aBlock
addShortcutWith: aBlock
addGroupWith: aBlock
```

`addGroupWith:` creates an anonymous subgroup. `addGroup:with:` registers the subgroup under an explicit id and reuses it if it already exists; this is the variant used to make groups addressable.

### 3.2 Ordering and ids

Two group features matter for real applications:

- **Priority ordering.** `SpActionGroup>>entries` sorts by priority. A fresh action has priority `0`; set `priority:` explicitly when you care about order. Core Spec does not break ties by name. Pulsar's collector adds a name tie-break at the root, so its menus are deterministic.
- **Ids.** `actionsFor: groupId` returns the direct subgroup with that id:

```smalltalk
actionsFor: groupId
    ^ self groups
        detect: [ :each | each id = groupId ]
        ifNone: [ self class new ]
```

If the id does not exist, you get a fresh empty group rather than an error. That is convenient, but it also hides typos. `actionsFor:` looks at direct subgroups; nested groups need to be reached through their parent.

There are no automatic separators keyed to priority. Priorities order entries; visual grouping comes from group boundaries and `beDisplayedAsGroup` / `beDisplayedAsSubMenu`.

### 3.3 Dynamic groups

Some menus cannot be described statically: their contents depend on the current selection or on the current state of the application. A `SpDynamicActionGroup` defers the construction of its content to the moment the menu is opened:

```smalltalk
root addDynamicGroupWith: [ :group |
    self windows do: [ :each |
        group addActionWith: [ :action |
            action
                name: each title;
                action: [ each raise ] ] ] ].
```

The block runs each time the menu is opened, so the entries always match the current selection.

### 3.4 How a group is displayed

Groups carry a display strategy. For menus:

- `beDisplayedAsGroup` flattens the group's actions into the current menu.
- `beDisplayedAsSubMenu` produces a submenu.
- `beRoot` marks the root group; the menu builder does not wrap the whole tree in an extra submenu (this is used internally, no needed for regular usage).

Toolbars have their own strategies:

- `beToolbarMenuButton` shows the group as a menu button (entries will be shown as menu entries).
- `beToolbarPopoverButton` shows it as a popover button.
- `beToolbarGroup` flattens it (the default toolbar behavior).

These toolbar strategies do not affect menus, and `beDisplayedAsSubMenu` does not drive toolbar submenus. Use the strategy that matches the surface.

---

## 4. Attaching actions to presenters

### 4.1 The `SpTActionContainer` trait

Every `SpPresenter` understands the action protocol through the `SpTActionContainer` trait. Its class comment states the contract:

> A trait to contain the actions associated to a presenter. Actions can be **visible** (hence, they will be shown in a context menu) or **not visible** (hence, they will be shortcuts).

The trait provides:

| Message | Effect |
| --- | --- |
| `actions` | The user-facing action group (`nil` until one is installed). |
| `actions: aGroup` | Installs a root user group and marks it `beRoot`. |
| `actionsWith: aBlock` | Creates a root group, lets the block fill it, installs it. |
| `addAction:` / `addActionWith:` | Adds one action to the user group. |
| `addShortcutWith:` | Adds a shortcut-only action. |
| `addActionGroup:` | Adds a group as a displayed-as-group child. |
| `addActionGroupWith:` | Creates a group and replaces the root group. |
| `internalActions` | Actions used internally by presenters (invisible). |
| `whenActionsChangedDo:` | Notification hook when the action group is replaced. |

The main group is marked `beRoot`. If no name is given, the root is named `#userActionGroup`.

### 4.2 User actions vs. internal actions

Not every action should be visible to the user. Text widgets, for example, need internal shortcuts (submit, show the context menu, …). Those live in a separate group: `internalActions`.

The backend installs both trees, but only user actions are intended to produce context menus. Internal actions are normally shortcut-only and remain invisible.

### 4.3 The keyboard-focus caveat

The API documents a limitation that can surprise newcomers. From `SpPresenter>>actions:`:

> **WARNING.** Defining actions in presenters that are not able to grab the keyboard focus is mostly useless (because they will never have the focus to answer to). Shortcut actions, however, can be installed and they will be available in children presenters when they are focused.

A `SpLabelPresenter` will never receive a keystroke, so a shortcut defined on a label is dead code. Shortcuts defined on a *container* still work when one of its children has the focus — but the most reliable pattern is to attach the actions to the widget that actually receives keyboard input (a text presenter, a list, a tree) or to the presenter that owns the focus.

### 4.4 A real example

In `StDependencyTreePresenter`, NewTools' dependency analyser attaches actions to a child tree, not to the presenter itself, and makes enablement depend on the current selection (trimmed):

```smalltalk
initializePresenters
    super initializePresenters.
    "..."
    tree actionsWith: [ :group |
        group addActionWith: [ :action |
            action
                name: 'Browse scoped dependencies';
                actionEnabled: [
                    (self areSubclassToPackageDependencyNodes: self selectedItemsFromTree)
                        ifTrue: [ self selectedPackageNames isNotEmpty ]
                        ifFalse: [ false ] ];
                action: [
                    (StPackageDependenciesPresenter onPackagesNamed: self selectedPackageNames)
                        application: self application;
                        open ] ] ].
```

The enablement block closes over `self`, so it does not need a context argument. The action still sees the same enablement logic when it is invoked from the menu or a shortcut.

---

## 5. How actions become UI

### 5.1 Same tree, different surfaces

Actions are not rendered directly. The presenter hands an action group to the active backend, which walks the tree and produces the target widgets:

- menu items for context menus and menu bars,
- buttons, toggles, menu buttons and popovers for toolbars,
- key bindings for shortcuts.

Because the tree is uniform, the same declaration can feed every surface. What differs per surface is which widget is produced and where it is installed. You normally do not call the renderer yourself; installing the group on the presenter is enough.

### 5.2 Menus and toolbars

Presenters that can hold a context menu (almost all of them) turn the group in `SpPresenter>>actions` into a context menu automatically. 
`SpMenuPresenter` and `SpToolbarPresenter` can also be filled manually:

```smalltalk
self newMenu fillWith: anActionGroup.
self newToolbar fillWith: anActionGroup.
```

Note: It is like this for historical reasons. In the future we will unify the API with `actions`.  

The same action object appears in the context menu, in the toolbar and as a shortcut. There is one enablement rule, one label and one icon.

### 5.3 Where the context comes from

This is the part that differs most between plain Spec and Pulsar.

| Situation | `action context` | Who sets it |
| --- | --- | --- |
| Plain Spec `actionsWith:` | `nil` | You: `action context: self`, or use zero-argument blocks |
| Pulsar `<dockActions>` | model/presenter | `PulsarActionCollector>>collect` |
| Legacy `rootCommandsGroup` | presenter | The legacy command-group path |

In plain Spec, `actionsWith:` does **not** set the context. If you write a one-argument block such as `actionEnabled: [ :aContext | aContext canRun ]`, it will receive `nil` and fail unless you set the context first.

In Pulsar, the collector sets every action's context to the collector's model, which defaults to the presenter it collected for. That is why Pulsar actions can be written as `action: [ :aContext | aContext doSomething ]`.

The legacy `rootCommandsGroup` / `editionCommandsGroup` path uses a different mechanism and is not the action path. Do not use it as a model for new action code.

### 5.4 Staying in sync

An action is a live object: its enablement and visibility may change with every selection change. The UI queries those hooks when it builds or refreshes the surface that shows the action. Menus are re-evaluated when opened. Toolbars may need the presenter to refresh before their buttons reflect a new state.

When the root action group itself is replaced, `whenActionsChangedDo:` lets the presenter react. It observes the `#userActionGroup` property. Mutating an existing action's name, icon or enablement block does not trigger that notification; refresh or rebuild the surface explicitly if you need the change to appear immediately.

### 5.5 Hide or disable?

When an action cannot run, the default is to show it **disabled**. This matches the usual UI guideline: show what a component can do, even if it is not available at the moment.

Two API areas change that behavior:

- `actionVisible:` hides the action independently of enablement.
- `beHiddenWhenCantBeRun` changes the display strategy so the action disappears when it cannot run; `beDisabledWhenCantBeRun` restores the default disabled appearance.

`actionEnabled:` does not itself hide anything; it answers whether the action can run. The current display strategy then decides whether to show it disabled or hide it.

---

## 6. Shortcuts

A shortcut is not a separate mechanism: it is an attribute of the action. Menus can display it, and the backend installs it as a native key binding. Toolbars normally show the label and icon, not the shortcut text.

```smalltalk
$a ctrl                                 "Ctrl+A"
$t shift actionModifier                 "Shift + platform action modifier"
$h mod1                                 "another common modifier form"
Character escape asKeyCombination       "Esc"
```

`$a ctrl` is the character `$a` with the Ctrl modifier. `$t shift actionModifier` adds Shift and the platform action modifier: Ctrl on Unix/Windows and Command on macOS.

A shortcut-only action is created with `beShortcutOnly` or `addShortcutWith:`:

```smalltalk
text addShortcutWith: [ :action |
    action
        shortcutKey: $s ctrl;
        action: [ self submit ] ]
```

The action never appears in a menu or toolbar, but it is installed on the text widget, which can receive the focus. If you attach the shortcut to a presenter that cannot receive keyboard focus, it will never fire; see [§4.3](#43-the-keyboard-focus-caveat).

---

## 7. Testing actions

Actions are objects, so they can be tested without opening a window. Spec's own tests document the two basic contracts: an action is registered in the presenter's group, and a shortcut-only action carries its key. In `SpAbstractTextPresenterTest`:

```smalltalk
testAddAction
    presenter addAction: (SpAction new
        name: 'test menu';
        action: [ ];
        yourself).

    self assert: presenter actions isNotNil.
    self assert: presenter actions allCommands isNotEmpty.
    self assert: (presenter actions allCommands anySatisfy: [ :each |
        each name = 'test menu' ]).
```

```smalltalk
testAddShortcut
    presenter addAction: (SpAction new
        beShortcutOnly;
        shortcutKey: $x ctrl;
        action: [ ];
        yourself).

    self assert: presenter actions isNotNil.
    self assert: presenter actions allCommands isNotEmpty.
    self assert: (presenter actions allCommands anySatisfy: [ :each |
        each shortcutKey = $x ctrl ]).
```

For enablement logic, the useful assertions are on `canBeExecuted`. Set the context explicitly:

```smalltalk
| action |
action := SpAction new
    name: 'Delete';
    actionEnabled: [ :aContext | aContext isNotEmpty ];
    action: [ ].

self deny: (action context: #(); canBeExecuted).
self assert: (action context: #(1); canBeExecuted).
```

If the action block itself matters, call `execute` and assert on the effect — but remember that `execute` is a no-op when `canBeExecuted` answers `false`, which is often exactly the behavior worth testing. `execute` also answers the action, not the block's return value.

---

## 8. How Pulsar uses actions

Pulsar is built on Spec, but it does not declare actions ad hoc inside every presenter. It adds a *convention* on top of the actions API, so that tools and extensions can contribute actions to presenters they do not own.

### 8.1 The `<dockActions>` convention

Pulsar actions are declared in methods carrying the `<dockActions>` pragma. The methods are usually named `define...ActionsOn:`, but the pragma is the contract; a few methods use a singular `...ActionOn:`.

Here is `PulsarPlayground>>defineActionsOn:` (trimmed):

```smalltalk
defineActionsOn: aBuilder
    <dockActions>

    aBuilder addGroup: #actions with: [ :group |
        group
            addActionWith: [ :action |
                action
                    priority: 1;
                    name: 'New playground';
                    description: 'Create a new playground';
                    iconName: 'add';
                    action: [ self doNewPlayground ] ];
            addActionWith: [ :action |
                action
                    priority: 2;
                    name: 'Load page';
                    description: 'Load a playground page';
                    iconName: 'load';
                    action: [ self doLoadPage ] ] ]
```

A collector finds those methods and folds them into one action tree per presenter:

- it searches the model's class hierarchy for `<dockActions>` methods, on the instance and class side,
- it builds one root group,
- it sorts the root entries by priority and then by name,
- it sets every action's context to the collector's model, which defaults to the presenter.

Pulsar is the reason the context examples in this article can use one-argument blocks.

In the image used for this article (Pharo 14, November 2026), there are **94** `<dockActions>` methods, and **9** of them are extension methods defined in another package. That is the practical payoff of the convention: a tool package can extend a presenter it does not own.

### 8.2 Hooking the tree into Spec

In `PulsarBaseView`, the common presenter base overrides the trait's `actions` accessor, so the tree is collected lazily the first time the framework asks for it:

```smalltalk
actions
    ^ actions ifNil: [ actions := PulsarActionCollector collectFor: self ]
```

This is the integration point with everything described above: the backend asks `presenter actions`, gets the collected tree, and installs it as if it had been declared with `actionsWith:`.

Internal actions still come from the trait; Pulsar does not override `ensureActions` or `internalActions`, so system shortcuts keep working unchanged.

### 8.3 Routing groups to UI areas

Because groups can be addressed by id, Pulsar separates the tree into areas without duplicating actions. The ids are independent; they are defined and consumed by different presenters:

| Group id | Defined in | Consumed by |
| --- | --- | --- |
| `#panels` | `PulsarBaseBrowser class>>definePanelActionsOn:` | `PulsarBaseBrowser>>initializeStatusbar` |
| `#panelWidget` | `PulsarEditor class>>defineWindowActionsOn:` | `PulsarEditor>>windowActions` |
| `#navigation` | `PulsarEditor>>defineNavigationActionsOn:` | `PulsarEditor>>editorActions` |

A real consumer for `#panels` is the status bar in `PulsarBaseBrowser`:

```smalltalk
initializeStatusbar
    self statusbar addSuffix: (self newToolbar
        displayMode: self application toolbarDisplayMode;
        fillWith: (self globalActions actionsFor: #panels))
```

The group itself is declared with the same pragma convention:

```smalltalk
definePanelActionsOn: aBuilder
    <dockActions>

    aBuilder addGroup: #panels with: [ :group |
        group
            priority: 1000;
            name: 'Panels';
            beDisplayedAsSubMenu.

        {
            { 'bottom'. SpPanelArea bottom }.
            { 'left'.   SpPanelArea start }.
            { 'right'.  SpPanelArea end }.
            { 'top'.    SpPanelArea top }
        } withIndexDo: [ :each :index |
            group addActionWith: [ :action |
                action
                    priority: index;
                    name: 'Toggle panel ', each first;
                    description: 'Show/hide the ', each first, ' panel';
                    iconName: 'panel-', each first;
                    action: [ :aContext |
                        aContext togglePanelArea: each second ] ] ] ]
```

`windowActions` and `editorActions` use the same pattern with their own ids. Keep the definitions and consumers together when reading the code; the ids are the contract.

### 8.4 Cross-package contributions

An extension method can add a whole group and reuse another model's actions. From `Pulsar-Tool-TestRunner`, extending the project model:

```smalltalk
defineTestRunnerActionOn: aBuilder
    <dockActions>

    aBuilder addGroup: #tools with: [ :group |
        group
            name: 'Tools';
            beDisplayedAsGroup;
            priority: 900;
            addActionWith: [ :action |
                action
                    name: 'Test Runner';
                    iconName: 'testNotRun';
                    description: 'Open test runner for this project';
                    shortcutKey: $t shift actionModifier;
                    action: [ :aContext |
                        aContext scheduleTask: [
                            (PulsarTestRunnerBrowser
                                newApplication: aContext application
                                model: self newTestRunnerModel)
                                open ] ] ] ]
```

`Pulsar-Tool-LLM` does the same on `PvClassModel`: it reuses an agent model's `#toolbar` actions and gates their visibility with `actionVisible:` on the `#llm` feature flag. The action machinery is plain Spec; Pulsar only adds the collection convention.

---

## 9. Migration and disambiguation

### 9.1 What changed

Older Spec documentation describes a different API. In this image:

- `SpPresenter class>>addTool:` does not exist,
- there is no `SpShortcut` class,
- there is no class-side tool registry.

The actions API replaces all of it: `SpAction` is the tool/shortcut, and `actions:` / `actionsWith:` replace `addTool:`. If you are porting code, move each tool declaration into `actionsWith:` (or a `<dockActions>` method in Pulsar) and translate `shortcut:` to `shortcutKey:`.

The legacy command-group layer still exists: `SpCommandGroup`, `SpPresenter>>rootCommandsGroup`, and the `/` navigation on command groups are loaded, and some tools still use them. For new code, prefer `SpActionGroup`.

`contextMenu:` and `contextKeyBindings:` are mutually exclusive with actions; using both signals an error. Do not mix the two mechanisms.

`initializePresenter` is obsolete. Its source says to use `connectPresenters` instead. Put presenter wiring in `initializePresenters` / `connectPresenters`, and action declarations in `actionsWith:` (or `initialize`).

### 9.2 Three command prefixes

Pharo has several command-ish frameworks, and the names collide. Check the prefix before assuming the API:

| Prefix | Framework | Role | Examples |
| --- | --- | --- | --- |
| `Cm*` | Commander2 (`Commander2`, `Commander2-UI`) | The command/decorator model under Spec actions. | `CmCommand`, `CmCommandGroup`, `CmUICommand` |
| `Cmd*` | Commander (`Commander-Core`, `Commander-Activators-*`) | Older menu/shortcut activation framework. | `CmdCommand`, `CmdShortcutActivation`, `CmdKMDispatcher`, `CmdToolContext` |
| `KM*` | Keymapping (`Keymapping-*`) | Key combination objects, keymaps and dispatchers. | `KMKeyCombination`, `KMKeymap` |

`CmdCommand` is not `CmCommand`. `CmdShortcutActivation` is not `SpAction`. `SpAction` lives in `Spec2-Commander2` and uses Commander2 underneath; Commander is unrelated to Spec2's actions.

### 9.3 Other things worth knowing

- **Icons are provided by the application.** `iconName:` is resolved through the action's icon provider. In a toolbar, if no provider is set, the toolbar presenter's application is used; otherwise the default theme icons apply. Pulsar installs an icon provider, which is why Pulsar actions use names such as `'hierarchy.neutral'` or `'testNotRun'`.
- **An action's identity matters.** Give actions stable, unique names or ids when you construct or reuse them programmatically. `assignUniqueName` on `SpCommand` gives a name to a shortcut-only command based on its id; it does not deduplicate names.
- **Order is priority, then name in Pulsar.** Core Spec sorts groups by priority only. Pulsar's collector adds the name tie-break at the root.

---

## 10. Recipes and pitfalls

### 10.1 A presenter with a menu, a toolbar and a shortcut

In `MyEditor`:

```smalltalk
initializePresenters
    super initializePresenters.
    text := self newText.

    self actionsWith: [ :root |
        root addGroup: #file with: [ :group |
            group
                name: 'File';
                beDisplayedAsGroup;
                addActionWith: [ :action |
                    action
                        name: 'Save';
                        iconName: #save;
                        shortcutKey: $s ctrl;
                        action: [ self save ] ] ] ].

    toolbar := self newToolbar.
    toolbar fillWith: self actions.
```

The same `Save` action appears in the context menu, in the toolbar, and answers Ctrl+S — with one enablement rule if you add `actionEnabled:` to it.

If `MyEditor` is not the widget that receives the keyboard focus, declare the shortcut on the text widget instead; see [§10.3](#103-a-shortcut-only-action-on-a-text-widget).

### 10.2 A dynamic group

```smalltalk
root addDynamicGroupWith: [ :group |
    self selectedItems do: [ :each |
        group addActionWith: [ :action |
            action
                name: 'Open ', each name;
                action: [ self open: each ] ] ] ].
```

The block runs each time the menu is opened, so the entries always match the current selection.

### 10.3 A shortcut-only action on a text widget

```smalltalk
text addShortcutWith: [ :action |
    action
        shortcutKey: $s ctrl;
        action: [ self submit ] ].
```

`addShortcutWith:` marks the action `beShortcutOnly`, so it never appears in a menu or toolbar but is installed on the text widget, which *can* receive the focus.

### 10.4 Common pitfalls

- **`action context` is `nil` in plain Spec.** A one-argument enablement/visibility/state block will receive `nil` unless you set the context. Use a zero-argument block or call `action context: self`.
- **A fresh action has priority `0`, not `1`.** Set `priority:` if you rely on ordering.
- **`actionsFor:` returns an empty group for an unknown id.** Check the id if a menu area is empty.
- **`contextMenu:` / `contextKeyBindings:` cannot be mixed with actions.** Pick one mechanism.
- **Shortcuts need keyboard focus.** Attach them to a text/list/tree or focusable presenter, not to a label.
- **`beDisplayedAsSubMenu` is for menus.** Use `beToolbarMenuButton` / `beToolbarPopoverButton` for toolbar submenus.
- **`whenActionsChangedDo:` observes group replacement, not action mutation.** Rebuild or refresh if you change an action in place.
- **`state:` is not a toggle.** Use `beToggleButton` for the toggle presenter.

---

## Small glossary

| Term | Meaning |
| --- | --- |
| **action** | An object describing something the user can do, with a label, behavior, enablement and optional shortcut/state. |
| **command** | The underlying executable behavior. An action is a command with UI decoration. |
| **action group** | A tree of actions and subgroups. Menus and toolbars traverse it. |
| **presenter** | A Spec object that owns a widget and exposes actions to it. |
| **backend** | The layer that renders Spec presenters with a particular widget toolkit. This article does not cover backend internals. |
| **context** | The object an action operates on. It is explicit in plain Spec and set by Pulsar's collector. |
| **shortcut-only** | An invisible action (`beShortcutOnly`) installed only as a key binding. |
| **`cull:`** | Sends as many arguments to a block as the block can accept. |
| **`yourself`** | Answers the receiver; used to end a cascade cleanly. |

---

## Where things live

| Class | Package | Role |
| --- | --- | --- |
| `SpAction` | `Spec2-Commander2` | An action built from a block. |
| `SpActionGroup`, `SpDynamicActionGroup` | `Spec2-Commander2` | Trees of actions; lazy groups. |
| `SpCommand`, `SpCommandGroup` | `Spec2-Commander2` | The command layer actions are built on. |
| `SpTActionContainer`, `SpPresenter` | `Spec2-Core` | Action attachment points. |
| `SpMenuPresenter`, `SpToolbarPresenter` | `Spec2-Core` | Widgets filled from action groups (`fillWith:`). |
| `CmCommand`, `CmCommandGroup` | `Commander2` | Commander2 core. |
| `CmUICommand`, `CmUICommandGroup` | `Commander2-UI` | UI decoration (icon, shortcut, strategy). |
| `PulsarActionCollector` | `Pulsar-Browser` | `<dockActions>` collection in Pulsar. |
