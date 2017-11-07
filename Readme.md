# mad-meetup-proxy-haskell

This is a simple web proxy for events for a specific group on api.meetup.com.

It doesn't accept any options, but it reads the following environment variables:

    PORT:           Set the port the web server should listen on.
                    Defaults to 3000.
    MEETUP_GROUP:   Set the group used in the requests to api.meetup.com.
                    Defaults to Fagkvelder-Itera.

The following endpoints exists:

    /               Currently redirects to /events/.
    /events         Lists events for the group. Accepts the status query parameter.
    /health         Reports the status of the service.
