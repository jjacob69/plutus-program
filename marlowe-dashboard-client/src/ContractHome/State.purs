module ContractHome.State
  ( dummyState
  , mkInitialState
  , handleAction
  , partitionContracts
  ) where

import Prelude
import Contract.State (isContractClosed)
import Contract.State (mkInitialState) as Contract
import Contract.Types (State) as Contract
import ContractHome.Lenses (_selectedContractIndex, _status)
import ContractHome.Types (Action(..), ContractStatus(..), State, PartitionedContracts)
import Data.Array as Array
import Data.Lens (assign)
import Data.Map (Map, mapMaybeWithKey)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Halogen (HalogenM)
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.PAB (ContractInstanceId, History)
import Marlowe.Semantics (Slot)

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = mkInitialState zero mempty

mkInitialState :: Slot -> Map ContractInstanceId History -> State
mkInitialState currentSlot contracts =
  { status: Running
  , contracts: mapMaybeWithKey (Contract.mkInitialState currentSlot) contracts
  , selectedContractIndex: Nothing
  }

handleAction ::
  forall m.
  Action -> HalogenM State Action ChildSlots Msg m Unit
handleAction OpenTemplateLibraryCard = pure unit -- handled in Play.State

handleAction (SelectView view) = assign _status view

handleAction (OpenContract ix) = assign _selectedContractIndex $ Just ix

partitionContracts :: Map ContractInstanceId Contract.State -> PartitionedContracts
partitionContracts contracts =
  Map.toUnfoldableUnordered contracts
    # map snd
    # Array.partition isContractClosed
    # \{ no, yes } -> { completed: yes, running: no }
