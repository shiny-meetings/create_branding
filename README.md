
# `create_branding`

## Project Description

An app that uses LLMS to:

- Generate a `_brand.yml` file after taking user inputs of
  company/personal branding. [This
  article](https://posit-dev.github.io/brand-yml/articles/llm-brand-yml-prompt/)
  shows how to prompt an LLM.  
- Generate `ggplot2` themes and scales using the information from the
  generated `_brand.yml` file.  
- Create a package structure that wraps the branding, scales, themes,
  etc. `fusen` package seems to be a decent way to do this.

Such a package may look something like the [`brandsw` example
package](https://github.com/durraniu/brandsw).

Other features might include:

- A package structure for Python.  
- Creating a quarto extension for the yaml file.

### Challenges

How to use LLMs in the app? Some possibilities:

- Use cloudflare workers AI models. Many models in beta are free to use
  with no limits. Other models have generous limits.  
- Ask the user for their key.

## Deliverables

**Start date: 2025-03-05**  
**End date: 2025-04-02**

- Day 1 and 2:
  - [ ] Initial wireframing. You may use AI tools such as
    [tldraw](https://www.tldraw.com/), [shiny
    assistant](https://gallery.shinyapps.io/assistant/#) or [ploomber AI
    editor](https://editor.ploomber.io/)
  - Framework to use:
    - [ ] shiny only
    - [ ] [golem](https://github.com/ThinkR-open/golem)
    - [ ] [rhino](https://github.com/Appsilon/rhino)
    - [ ] shiny + [ambiorix](https://ambiorix.dev/)
- Day 3:
  - [ ] First meeting for discussing UI and features  
- Day 10:
  - [ ] Second meeting for discussing <features>  
- Day 17:
  - [ ] Third meeting for discussing <features>  
- Day 24:
  - [ ] Final meeting for discussing <features> and deployment
- Day 29 or before:
  - [ ] Deployment

## Meetings

Projects start on Wednesdays with meetings on Fridays. We have a shiny
club channel on the [DSLC
Slack](https://dslcio.slack.com/archives/C08A52V98TY) for chatting. Zoom
link appears 10 minutes before meeting on Fridays in the slack channel.
If you’re not a member of DSLC, [join here](https://dslc.io/join) or you
will be sent a meeting link.

## How to Contribute

- Fork this repository
- Create a new branch for your feature
- Make your changes
- Submit a pull request
- Wait for review and address any feedback
