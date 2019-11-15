module TodoApp where

import Prelude
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic as RB
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, stopPropagation, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Events as Events
import React.Basic.Hooks (ReactComponent, component, element, elementKeyed, empty, memo, useReducer, useState, (/\))
import React.Basic.Hooks as React

data Action
  = CreateTodo String
  | ToggleTodo Int
  | DeleteTodo Int
  | SetFilter TodoFilter

data TodoFilter
  = All
  | Complete
  | Incomplete

derive instance eqTodoFilter :: Eq TodoFilter

type Todo
  = { task :: String
    , isComplete :: Boolean
    }

type State
  = { todos :: Array Todo
    , filter :: TodoFilter
    }

reducer :: State -> Action -> State
reducer state = case _ of
  CreateTodo task -> state { todos = Array.cons { task, isComplete: false } state.todos }
  ToggleTodo index -> case Array.modifyAt index (\todo -> todo { isComplete = not todo.isComplete }) state.todos of
    Just todos -> state { todos = todos }
    Nothing -> state
  DeleteTodo index -> case Array.deleteAt index state.todos of
    Just todos -> state { todos = todos }
    Nothing -> state
  SetFilter filter -> state { filter = filter }

mkTodoApp :: Effect (ReactComponent {})
mkTodoApp = do
  let
    initialState = { todos: [], filter: All }
  todoInput <- memo mkTodoInput
  todoRow <- memo mkTodoRow
  todoFilters <- memo mkTodoFilters
  component "TodoApp" \props -> React.do
    state /\ dispatch <- useReducer initialState reducer
    pure
      $ R.div
          { children:
            [ element todoInput { dispatch }
            , R.div_
                $ flip Array.mapWithIndex state.todos \id todo ->
                    if state.filter == All
                      || (todo.isComplete && state.filter == Complete)
                      || (not todo.isComplete && state.filter == Incomplete) then
                      elementKeyed todoRow { key: show id, id, todo, dispatch }
                    else
                      empty
            , element todoFilters { filter: state.filter, dispatch }
            ]
          , style:
            R.css
              { maxWidth: "600px"
              , margin: "auto"
              , padding: "16px"
              , fontFamily: "sans-serif"
              , fontSize: "16px"
              }
          }
  where
  todoAppEl = RB.element $ R.unsafeCreateDOMComponent "todo-app"

mkTodoInput :: Effect (ReactComponent { dispatch :: Action -> Effect Unit })
mkTodoInput = do
  component "TodoInput" \props -> React.do
    value /\ setValue <- useState ""
    pure
      $ R.form
          { onSubmit:
            handler (preventDefault >>> stopPropagation) \_ -> do
              props.dispatch $ CreateTodo value
              setValue $ const ""
          , children:
            [ R.input
                { value
                , onChange:
                  handler (preventDefault >>> stopPropagation >>> targetValue)
                    $ traverse_ (setValue <<< const)
                , style: R.css { lineHeight: "32px", width: "100%", boxSizing: "border-box" }
                , placeholder: "Enter a task"
                }
            ]
          , style: R.css { marginBottom: "16px", width: "100%" }
          }

mkTodoRow :: Effect (ReactComponent { id :: Int, todo :: Todo, dispatch :: Action -> Effect Unit })
mkTodoRow =
  component "Todo" \props -> React.do
    pure
      $ R.div
          { children:
            [ R.label
                { children:
                  [ R.input
                      { type: "checkbox"
                      , checked: props.todo.isComplete
                      , onChange: Events.handler_ $ props.dispatch $ ToggleTodo props.id
                      , tabIndex: 0
                      }
                  , R.text props.todo.task
                  ]
                , style: R.css { lineHeight: "32px", fontSize: "24px", flex: "1 0 auto" }
                }
            , R.a
                { children: [ R.text "❌" ]
                , onClick: handler_ $ props.dispatch $ DeleteTodo props.id
                , style: R.css { cursor: "pointer" }
                }
            ]
          , style:
            R.css
              { display: "flex"
              , flexFlow: "row"
              , alignItems: "center"
              }
          }

mkTodoFilters :: Effect (ReactComponent { filter :: TodoFilter, dispatch :: Action -> Effect Unit })
mkTodoFilters =
  component "TodoFilters" \props -> React.do
    let
      filterLink f label =
        R.a
          { children: [ R.text label ]
          , onClick: handler_ $ props.dispatch $ SetFilter f
          , style:
            if props.filter == f then
              R.css { cursor: "pointer", fontWeight: "bold" }
            else
              R.css { cursor: "pointer" }
          }
    pure
      $ R.div
          { children:
            [ R.hr { style: R.css { color: "lightgrey" } }
            , filterLink All "All"
            , R.text " / "
            , filterLink Complete "Complete"
            , R.text " / "
            , filterLink Incomplete "Incomplete"
            ]
          , style: R.css { marginTop: "16px" }
          }
