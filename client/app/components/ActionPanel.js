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
        {gameStage === 'PreDeal' ?
          <React.Fragment>
            <button
              type="button"
              onClick={() => sitDown(betValue)}
              className="button">
              Sit Down Bet <span className='monospaced-font'>
                {betValue}</span>
            </button>
            <button
              type="button"
              onClick={() => postBigBlind()} className="button">
              Post Big Blind
          </button>
            <button
              type="button"
              onClick={() => leaveGameSeat()}
              className="button">
              Leave Seat
          </button>
            <button
              type="button" onClick={() => postSmallBlind()} className="button">
              post Small Blind
          </button>
          </React.Fragment>
          : ''}
        {gameStage !== 'Showdown' && gameStage !== 'PreDeal' && isTurnToAct ?
          <React.Fragment>
            <button type="button" onClick={() => check()} className="button">
              Check
          </button>
            <button type="button" onClick={() => call()} className="button">
              Call</button>
            <button
              type="button"
              onClick={() => bet(betValue)} className="button">Bet <span className='monospaced-font'>
                {betValue}</span></button>
            <button
              type="button"
              onClick={() => raise(betValue)}
              className="button">
              Raise {betValue}</button>
            <button
              type="button"
              onClick={() => fold()}
              className="button">
              Fold
          </button>
          </React.Fragment>
          : ''}
        <button
          type="button"
          onClick={() => sitIn()}
          className="button">
          Sit In
          </button>

      </div>
    </div>)
}


export default ActionPanel
