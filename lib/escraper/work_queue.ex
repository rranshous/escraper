defmodule Escraper.WorkQueue do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :workqueue }, __MODULE__, [], [])
  end

  def init(queue) do
    { :ok, queue }
  end

  def handle_cast({ :add, page }, queue) do
    IO.puts "work queue adding: #{page.url}"
    :gen_server.cast(:pagescraper, :work_added)
    { :noreply, [page|queue] }
  end

  def handle_call(:pop, _from, [h|queue]) do
    { :reply, h, queue }
  end
end
