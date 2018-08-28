import React from 'react';

const Seat = ({ playerName, chips, isTurnToAct, playerState, position }) => (
  <div className={`seat-${position}-container`}>
    {playerState === 'In' ?
      <div className='hidden-pocket-cards' >
        <div className='card pocket-one' />
        <div className='card pocket-two' />
      </div> : ''}
    <div className={`seat-${position}`}>
      <h2>{playerName || 'Seat'}</h2>
      {playerName ? <h2>{chips}</h2> : ''}
    </div>
  </div>);

export default Seat;