import React from 'react';

import Heart from '../images/Hearts.svg';

const showRank = rank => {
  switch (rank) {
    case 'Ace':
      return 'A'
    case 'King':
      return 'K'
    case 'Queen':
      return 'Q'
    case 'Jack':
      return 'J'
    case 'Ten':
      return '10'
    case 'Nine':
      return '9'
    case 'Eight':
      return '8'
    case 'Seven':
      return '7'
    case 'Six':
      return '6'
    case 'Five':
      return '5'
    case 'Four':
      return '4'
    case 'Three':
      return '3'
    case 'Two':
      return '2'
  }
}

const Card = ({ rank, suit }) => (
  <div className="card">
    <div className="rank">
      {showRank(rank)}
    </div>
    <div className="suit">
      <Heart height={150} />
    </div>
  </div>);

export default Card;


//      <img alt={suit} src={`${suit}.svg`} />
