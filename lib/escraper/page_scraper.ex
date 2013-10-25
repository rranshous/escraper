defmodule Escraper.PageScraper do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :pagescraper }, __MODULE__, [], [])
  end

  def init(log) do
    { :ok, HashSet.new(log) }
  end

  def handle_cast({ :work_added, page }, log) do
    if(! HashSet.member?(log, page.url)) do
      log = HashSet.put(log, page.url)
      spawn_scraper page
    else
      IO.puts "skipping work, in progress: #{page.url}"
    end
    { :noreply, log }
  end

  def handle_cast({ :work_completed, url }, log) do
    #{ :noreply, HashSet.delete(log, url) }
    { :noreply, log }
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

    :gen_server.cast(:pagescraper, { :work_completed, page.url })
  end

  def parse_links(page) do
    links = Escraper.Helpers.parse_links_from_html page.body
    links = Enum.map links, &resolve_relative_links(&1, page.root_url)
    IO.puts "links [#{page.url}]: #{length(links)}"
    links
  end

  def resolve_relative_links(url, root_url) do
    if(String.starts_with?(url, "/")) do
      if(String.ends_with?(root_url, "/")) do
        url = String.slice(root_url, 0, String.length(root_url)-1) <> url
      else
        url = root_url <> url
      end
    else
      if(String.slice(url,0,4) != "http") do
        if(String.at(url,0) == ".") do
          url = String.slice(url,1,-1)
        end
        if(String.at(url,0) == "/") do
          url = root_url <> url
        else
          url = root_url <> "/" <> url
        end
      end
    end
    url
  end
end
