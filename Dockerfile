FROM haskell:8

WORKDIR /opt/server

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./meetup-proxy.cabal /opt/server/meetup-proxy.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal install --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/server
RUN cabal install

EXPOSE 3000

CMD ["meetup-proxy"]
