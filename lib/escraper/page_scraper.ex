defmodule Escraper.PageScraper do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :pagescraper }, __MODULE__, [], [])
  end

  def init(state) do
    { :ok, state }
  end

  def handle_cast(:work_added, state) do
    case :gen_server.call(:workqueue, :pop) do
      { :ok, page } ->
        spawn_scraper page

      { :error, :empty } -> nil
    end
    { :noreply, state }
  end

  def handle_cast({ :work_added, page }, state) do
    spawn_scraper page
    { :noreply, state }
  end

  def spawn_scraper(page) do
    spawn(Escraper.PageScraper, :do_scrape, [page])
  end

  def do_scrape(page) do
    IO.puts "doing scrape: #{page.url}"
    case :hackney.request(:get, page.url, [], [], []) do

      { :ok, status, headers, c } -> 
        { :ok, body, c2 } = :hackney.body(c)
        page = page.body(body)
        page = page.status(status)
        if(String.length(page.body) == 0 || page.status == 500) do
          IO.puts "is bad [#{page.url}]: #{page.status}"
          :gen_server.cast(:sitescraper, { :add_page, page.failed(true) })
        else
          if(Escraper.Helpers.data_is_html?(body)) do
            page = page.links(parse_links(page))
            page = page.is_html(true)
            :gen_server.cast(:sitescraper, { :add_page, page })
          else
            IO.puts "is not html: #{page.url}"
            :gen_server.cast(:sitescraper, { :add_page, page.is_image(true) })
          end
        end

      { :error, reason } ->
        IO.puts "request failed: #{page.url}"
        :gen_server.cast(:sitescraper, { :add_page, page.failed(true) })
    end
  end

  def parse_links(page) do
    links = Escraper.Helpers.parse_links_from_html page.body
    IO.puts "links [#{page.url}]: #{length(links)}"
    links
  end

end
