defmodule Escraper.PageScraper do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :pagescraper }, __MODULE__, [], [])
  end

  def init(state) do
    { :ok, state }
  end

  def handle_cast(:work_added, state) do
    IO.puts "work was added"
    page = :gen_server.call(:workqueue, :pop)
    IO.puts "work: #{page.url}"
    spawn_scraper page
    { :noreply, state }
  end

  def spawn_scraper(page) do
    IO.puts "spawning scraper: #{page.url}"
    spawn(Escraper.PageScraper, :do_scrape, [page])
  end

  def do_scrape(page) do
    IO.puts "doing scrape: #{page.url}"
    { :ok, status, headers, c } = :hackney.request(:get, page.url, [], [], [])
    { :ok, body, c2 } = :hackney.body(c)
    IO.puts "scrape status: #{status}"
    page = page.body(body)
    IO.puts "pushing page: #{page.url} :: #{String.length(page.body)}"
    :gen_server.cast(:sitescraper, { :add_page, page })
  end

end
