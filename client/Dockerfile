# base image
FROM node:10.16.3-alpine

RUN apk add --no-cache \
    autoconf \
    automake \
    bash \
    g++ \
    libc6-compat \
    libjpeg-turbo-dev \
    libpng-dev \
    make \
    nasm

WORKDIR /app

RUN npm install yarn@1.17.3 -g

# install and cache app dependencies
COPY package.json .

RUN yarn

COPY . .

CMD ["yarn", "start"]
