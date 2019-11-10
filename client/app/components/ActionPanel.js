import React from 'react'

// TODO move to own component called pocket cards
import Card from './Card'

const getPocketCards = cards =>
  cards !== undefined && cards !== null ? cards.map(card => {
    const rank = card.get('rank')
    const suit = card.get('suit')

    return (<Card
      key={rank + suit}
      rank={rank}
      suit={suit}
    />)
  }) : ''




const ActionPanel = ({
  handleChange,
  betValue,
  bet,
  raise,
  call,
  fold,
  check,
  postSmallBlind,
  postBigBlind,
  sitDown,
  leaveGameSeat,
  userPocketCards,
  gameStage,
  sitIn,
  isTurnToAct,
  availableActions,
}) => {
  console.log('available actions', availableActions)


  const preDealActions =
    gameStage === "PreDeal" ? <React.Fragment>

      {availableActions.includes("PostBigBlind") ?
        <button
          type="button"
          onClick={() => postBigBlind()} className="button">
          Post Big Blind
      </button> : ''}

      {availableActions.includes("PostSmallBlind") ?
        <button
          type="button" onClick={() => postSmallBlind()} className="button">
          post Small Blind
      </button> : ' '}

      <button
        type="button"
        onClick={() => sitDown(betValue)}
        className="button">
        Sit Down Bet <span className='monospaced-font'>
          {betValue}</span>
      </button>

      <button
        type="button"
        onClick={() => sitIn()}
        className="button">
        Sit In
          </button>

      <button
        type="button"
        onClick={() => leaveGameSeat()}
        className="button">
        Leave Seat
    </button>

    </React.Fragment> : '';

  return (
    <div className='action-panel'>
      <div className='action-panel-left-container'>
        {userPocketCards ? userPocketCards.map(card => {
          const rank = card.get('rank')
          const suit = card.get('suit')

          return (<Card
            key={rank + suit}
            rank={rank}
            suit={suit}
          />)
        }) : ''}
      </div>
      <div className='user-actions-container'>
        {gameStage === 'PreDeal' || (gameStage !== 'PreDeal' && isTurnToAct) ?
          <input
            type="text"
            value={betValue}
            onChange={handleChange}
          /> : ''}
        {preDealActions}
        {true ?   // gameStage !== 'Showdown' && gameStage !== 'PreDeal' && isTurnToAct ?
          <React.Fragment>

            {availableActions.includes("Check") ?
              <button type="button" onClick={() => check()} className="button">
                Check
       </button> : ''}


            {availableActions.includes("Call") ?
              <button type="button" onClick={() => call()} className="button">
                Call</button> : ''}

            {availableActions.includes("Bet") ? <button
              type="button"
              onClick={() => bet(betValue)} className="button">Bet <span className='monospaced-font'>
                {betValue}</span></button> : ''}
            {availableActions.includes("Raise") ?
              <button
                type="button"
                onClick={() => raise(betValue)}
                className="button">
                Raise {betValue}</button> : ''}
            {availableActions.includes("Fold") ? <button
              type="button"
              onClick={() => fold()}
              className="button">
              Fold
          </button> : ''}
          </React.Fragment>
          : ''}


      </div>
    </div>)
}


export default ActionPanel
